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

entity merkle_top is
  generic(
    n_leaves  : integer := 8;
    s_width : integer := 32;
    p_width : integer := 256;
    logn : integer := 3;
    logp : integer := 3
  );
  port(
    clk       : in  std_logic;
    rst_n     : in  std_logic;
    s_in      : in  std_logic_vector(s_width-1 downto 0);
    s_go      : in  std_logic;  
    in_ok			: in std_logic;  
    serial_out  : out std_logic_vector(s_width-1 downto 0);
    out_word_idx  : out std_logic_vector(logn-1 downto 0);
    out_part_idx  : out std_logic_vector(logp-1 downto 0);
    hash_done : out std_logic := '0'
  );
end merkle_top;

architecture rtl of merkle_top is
  component merkle_tree is
    generic(
      leaves_nbr  : integer range 0 to 15 := 8;
      logl        : integer := 3      
    );
    port(
      clk         : in  std_logic;
      rst_n       : in  std_logic;
      tree_go     : in  std_logic;
      leaves      : in  leaves_t(0 to leaves_nbr-1);
      out_word    : out std_logic_vector(255 downto 0);
      out_node_idx: out std_logic_vector(logl-1 downto 0);
      out_ok      : out std_logic
    );
  end component merkle_tree;
  
  component merkle_sp is
    generic(
      s_width     : integer := s_width;
      p_width     : integer := p_width;
      n_leaves    : integer := 8 
    );
    port(
      clk         : in  std_logic;
      rst_n       : in  std_logic;
      l_go        : in  std_logic;
      finished    : out std_logic := '0';
      s_in        : in  std_logic_vector(s_width-1 downto 0);
      p_out       : out std_logic_vector(n_leaves*p_width-1 downto 0) := (others => '0');
      in_ok				: in  std_logic   
    );
  end component merkle_sp;
  
  component merkle_ps is
    generic(
      s_width     : integer := 32;
      p_width     : integer := 256;
      n_out       : integer := 8;
      logp        : integer := 3;
      logn        : integer := 3
    );
    port(
      clk         : in  std_logic;
      rst_n       : in  std_logic;
      out_go      : in  std_logic;
      out_ok      : out std_logic := '0';
      out_part_idx: out std_logic_vector(logp-1 downto 0);
      p_in        : in  std_logic_vector(p_width-1 downto 0);
      s_out       : out std_logic_vector(s_width-1 downto 0) := (others => '0')
    );
  end component merkle_ps;
	
  signal s_tree_go : std_logic := '0';
  signal s_leaves : leaves_t(0 to n_leaves-1) := (others => (others => '0'));
  signal s_blk_leaves : std_logic_vector(n_leaves*p_width-1 downto 0) := (others => '0');
  signal s_out_word    : std_logic_vector(255 downto 0);
  signal s_out_ok      : std_logic;
  signal s_out_part_idx:  std_logic_vector(2 downto 0);
  signal s_out_node_idx:  std_logic_vector(2 downto 0);

begin
  tree_comp : merkle_tree
    generic map(
      leaves_nbr => n_leaves,
      logl => 3
    )
    port map(
      clk => clk,
      rst_n => rst_n,
      tree_go => s_tree_go,
      leaves => s_leaves,
      out_word => s_out_word,
      out_node_idx => s_out_node_idx,
      out_ok => s_out_ok
    );
  
  leaves_comp : merkle_sp
    generic map(
      s_width => s_width,
      p_width => p_width,
      n_leaves => n_leaves
    )
    port map(
      clk => clk,
      rst_n => rst_n,
      l_go => s_go,
      finished => s_tree_go,
      p_out => s_blk_leaves,
      s_in  => s_in,
      in_ok  => in_ok
    );
    
    g_leaves : for i in 0 to n_leaves-1 generate
      s_leaves(i) <= s_blk_leaves((i+1)*p_width-1 downto i*p_width);
    end generate;
    
    out_comp : merkle_ps
    generic map(
      s_width => s_width,
      p_width => p_width,
      n_out => n_leaves,
      logp => logp,
      logn => logn
    )
    port map(
      clk => clk,
      rst_n => rst_n,
      out_go => s_out_ok,
      out_ok => hash_done,
      out_part_idx => s_out_part_idx,
      p_in => s_out_word,
      s_out => serial_out
    );
    
    out_word_idx <= s_out_node_idx;
    out_part_idx <= s_out_part_idx;
end rtl;