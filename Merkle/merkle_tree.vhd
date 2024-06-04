-- Copyright CEA
-- Romain Michard 2024/02/21
-- romain.michard_AT_m4x.org
--
-- This software is a synthesizable VHDL implementation of a Merkle tree. The -- user can choose the size by use of generics.
--
-- This software is governed by the CeCILL license under French law and
-- abiding by the rules of distribution of free software. You can use,
-- modify and/or redistribute the software under the terms of the CeCILL
-- license as circulated by CEA, CNRS and INRIA at the following URL
-- "http://www.cecill.info".
-- 
-- As a counterpart to the access to the source code and rights to copy,
-- modify and redistribute granted by the license, users are provided only
-- with a limited warranty and the software's author, the holder of the
-- economic rights, and the successive licensors have only limited
-- liability.
-- 
-- In this respect, the user's attention is drawn to the risks associated
-- with loading, using, modifying and/or developing or reproducing the
-- software by the user in light of its specific status of free software,
-- that may mean that it is complicated to manipulate, and that also
-- therefore means that it is reserved for developers and experienced
-- professionals having in-depth computer knowledge. Users are therefore
-- encouraged to load and test the software's suitability as regards their
-- requirements in conditions enabling the security of their systems and/or
-- data to be ensured and, more generally, to use and operate it in the
-- same conditions as regards security.
-- 
-- The fact that you are presently reading this means that you have had
-- knowledge of the CeCILL license and that you accept its terms.

library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.merkle_pkg.all;

entity merkle_tree is
  generic(
    leaves_nbr  : integer range 0 to 16 := 8;    -- the number of input leaves (must be a power of 2)
    logl        : integer range 0 to 4 := 3      -- log2 of the leaves number
  );
  port(
    clk         : in  std_logic;									-- clock
    rst_n       : in  std_logic;									-- reset, active low
    tree_go     : in  std_logic;                  -- to start the computation
    leaves      : in  leaves_t(0 to leaves_nbr-1);-- leaves of the tree
    out_word    : out std_logic_vector(255 downto 0);--
    out_node_idx: out std_logic_vector(logl-1 downto 0);
    out_ok      : out std_logic
  );
end merkle_tree;

architecture rtl of merkle_tree is

  component merkle_pll IS
    PORT
    (
      areset		: IN STD_LOGIC  := '0';
      inclk0		: IN STD_LOGIC  := '0';
      c0		: OUT STD_LOGIC 
    );
  END component merkle_pll;

  component merkle_node is    -- the basic component
    port(
      clk         : in  std_logic;
      rst_n       : in  std_logic;
      start_l     : in  std_logic;
      start_r     : in  std_logic;
      node_done   : out std_logic;
      n_l         : in  std_logic_vector(255 downto 0);          -- left child
      n_r         : in  std_logic_vector(255 downto 0);          -- right child
      node_out    : out std_logic_vector(255 downto 0);
      
      ------------------------- 
      -- keccak interface
      -------------------------
      k_init      : out std_logic;
      k_absorb    : out std_logic;
      k_go        : out std_logic;
      k_squeeze   : out std_logic;
      k_ready     : in  std_logic;
      k_din       : out std_logic_vector(63 downto 0);
      k_dout      : in  std_logic_vector(63 downto 0);
      
      k_att       : in  std_logic
    );
  end component merkle_node;
  
  component keccak is     -- BLISS keccak component
  port (
    clk         : in  std_logic;
    rst_n       : in  std_logic;
    init        : in  std_logic;
    go          : in  std_logic;
    absorb      : in  std_logic;
    squeeze     : in  std_logic;
    din         : in  std_logic_vector(63 downto 0);    
    ready       : out std_logic;
    dout        : out std_logic_vector(63 downto 0));
  end component;

  type hash_array is array(0 to 2*leaves_nbr-1) of std_logic_vector(255 downto 0);  -- a hash line
  signal s_hash : hash_array := (others => (others => '0')); -- array of the hash words

  -- the same to handle the signals in the tree
  signal s_done, s_init, s_go, s_absorb, s_squeeze, s_ready, s_k_att : std_logic_vector(leaves_nbr-1 downto 1) := (others => '0');
  type data_array is array(leaves_nbr-1 downto 1) of std_logic_vector(63 downto 0);
  signal s_din, s_dout : data_array;

  -- signal to/from keccak
  signal s_k_init, s_k_go, s_k_absorb, s_k_squeeze, s_k_ready :  std_logic;
  signal s_k_din, s_k_dout : std_logic_vector(63 downto 0);

  -- counter telling which node is actually computed  
  signal n_cntr : integer range 1 to leaves_nbr-1 := leaves_nbr-1;
  
  signal r_hash_done : std_logic := '0';
 
  -- because the hash words are 256-bit wide but the FPGA output is 32-bit wide
  signal clock2 : std_logic;
  signal counter_c2 : integer range 0 to 3 := 0;
  signal arst : std_logic;
  
  -- to have indices at the right time
  signal node_idx_t1 : std_logic_vector(logl-1 downto 0) := (others => '0');
  signal node_idx_t2 : std_logic_vector(logl-1 downto 0) := (others => '0');
    
begin
  arst <= not(rst_n);
  merkle_pll_inst : merkle_pll PORT MAP (
		areset	 => arst,
		inclk0	 => clk,
		c0	 => clock2
	);

  p_leaves_as_hash : for i in leaves_nbr to 2*leaves_nbr-1 generate
    s_hash(i) <= leaves(i-leaves_nbr);
  end generate;
  
  stages_loop : for y in 1 to logl-1 generate		-- construction of the tree
    column : for x in 0 to 2**(y-1)-1 generate
      loop_node : merkle_node		-- instanciation of the nodes
        port map(
          clk => clk,
          rst_n => rst_n,
          start_l => s_done(2**y+2*x),
          start_r => s_done(2**y+2*x+1),
          node_done => s_done(2**(y-1)+x),
          n_l => s_hash(2**y+2*x),
          n_r => s_hash(2**y+2*x+1),
          node_out => s_hash(2**(y-1)+x),
          
          ------------------------- 
          -- to or from keccak
          -------------------------
          k_init => s_init(2**(y-1)+x),
          k_absorb => s_absorb(2**(y-1)+x),
          k_go => s_go(2**(y-1)+x),
          k_squeeze => s_squeeze(2**(y-1)+x),
          k_ready => s_ready(2**(y-1)+x),
          k_din => s_din(2**(y-1)+x),
          k_dout => s_dout(2**(y-1)+x),
          
          k_att => s_k_att(2**(y-1)+x)
        );
    end generate;
  end generate;
  
  last_stage : for x in 0 to leaves_nbr/2-1 generate	-- a different stage for the leaves
    last_line : merkle_node
      port map(
        clk => clk,
        rst_n => rst_n,
        start_l => tree_go,
        start_r => tree_go,
        node_done => s_done(leaves_nbr/2+x),
        n_l => leaves(2*x),
        n_r => leaves(2*x+1),
        node_out => s_hash(leaves_nbr/2+x),
        ------------------------- 
        -- to or from keccak
        -------------------------
        k_init => s_init(leaves_nbr/2+x),
        k_absorb => s_absorb(leaves_nbr/2+x),
        k_go => s_go(leaves_nbr/2+x),
        k_squeeze => s_squeeze(leaves_nbr/2+x),
        k_ready => s_ready(leaves_nbr/2+x),
        k_din => s_din(leaves_nbr/2+x),
        k_dout => s_dout(leaves_nbr/2+x),
        
        k_att => s_k_att(leaves_nbr/2+x)
      );
  end generate;

  k_comp : keccak
    port map(
      clk         => clk,
      rst_n       => rst_n,
      init        => s_k_init,
      absorb      => s_k_absorb,
      squeeze     => s_k_squeeze,
      go          => s_k_go,
      din         => s_k_din,  
      ready       => s_k_ready,
      dout        => s_k_dout
    );

  s_k_init <= s_init(n_cntr);
  s_k_absorb <= s_absorb(n_cntr);
  s_k_squeeze <= s_squeeze(n_cntr);
  s_k_go <= s_go(n_cntr);
  s_k_din <= s_din(n_cntr);
  output_keccak_loop : for k in 1 to leaves_nbr-1 generate
    s_ready(k) <= s_k_ready;
    s_dout(k) <= s_k_dout;
  end generate;
  
  p_out: process(clk, rst_n)
  begin
    if rst_n = '0' then
      r_hash_done <= '0';
    elsif rising_edge(clk) then
      if s_done(1)='1' then
        r_hash_done <= '1';
      end if;
    end if;
  end process;
  
  tree_main: process(clk, rst_n, n_cntr)
  begin
    if rst_n = '0' then                 -- asynchronous rst_n (active low)
      n_cntr <= leaves_nbr-1;
    elsif rising_edge(clk) then  -- rising clk edge
      if tree_go = '1' then
        n_cntr <= leaves_nbr-1;
      else
        if s_done(n_cntr)='1' then
          if n_cntr>1 then
            n_cntr <= n_cntr - 1;  -- n_cntr = 2^(n_y-1) + n_x
          end if;
        end if;
      end if;
    end if;
    
    k_attr : for k in 1 to leaves_nbr-1 loop
      if k=n_cntr then
        s_k_att(k) <= '1';
      else
        s_k_att(k) <= '0';
      end if;
    end loop;
  end process;
  
  out_process: process(rst_n, clock2, s_hash, node_idx_t1, node_idx_t2)
    variable n : integer range 0 to leaves_nbr-1;
  begin
    if rst_n = '0' then
      n := 0;
      out_ok <= '0';
      node_idx_t1 <= (others => '0');
      node_idx_t2 <= (others => '0');
      out_node_idx <= (others => '0');
      out_word <= (others => '0');
    elsif rising_edge(clock2) then
      if r_hash_done='1' and n<leaves_nbr-1 then
        n := n+1;
        out_ok <= '1';
      else
        out_ok <= '0';
      end if;
    end if;
    node_idx_t1 <= std_logic_vector(to_unsigned(n, out_node_idx'length));
    node_idx_t2 <= node_idx_t1;
    out_node_idx <= node_idx_t2;
    out_word <= s_hash(n);
  end process;
    
end rtl;