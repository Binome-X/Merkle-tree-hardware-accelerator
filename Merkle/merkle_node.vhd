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

entity merkle_node is
  port(
    clk         : in  std_logic;															-- clock
    rst_n       : in  std_logic;															-- asynchronous reset, active low
    start_l     : in  std_logic;                              -- left child is ok and tells the module it can use it
    start_r     : in  std_logic;                              -- right child is ok and tells the module it can use it
    node_done   : out std_logic := '0';                       -- hash finished
    n_l         : in  std_logic_vector(255 downto 0);         -- left child
    n_r         : in  std_logic_vector(255 downto 0);         -- right child
    node_out    : out std_logic_vector(255 downto 0) := (others => '0' );         -- hash result
    
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
    
    k_att       : in  std_logic     -- Keccak attributed to this node
  );
end merkle_node;

architecture rtl of merkle_node is

  -- signals to control the keccak component
  signal node_init,node_go,node_absorb,node_ready,node_squeeze,node_start,s_node_done : std_logic; 
  signal node_din,node_dout : std_logic_vector(63 downto 0);
  
  -- state signal of the fsm
  type st_type is (initial,read_first_input,st0,st1,st1a,END_HASH1,END_HASH2,stop_k);
  signal st : st_type;
  
  -- used to count various cycles
  signal counter : integer range 0 to 15;
  
  -- registers to memorize the start values
  signal r_start_l, r_start_r : std_logic;
  
begin
  start_registers : process(clk,rst_n) -- memorize the start values
  begin
    if rst_n = '0' then
      r_start_l <= '0';
      r_start_r <= '0';
    elsif rising_edge(clk) then
      if start_l = '1' then
        r_start_l <= '1';
      end if;
      if start_r = '1' then
        r_start_r <= '1';
      end if;
    end if;
  end process;
  
  node_start <= r_start_l and r_start_r and k_att;     -- start only when both children are ready and the Keccak component is available for me
  node_done <= s_node_done;

  -- keccak control
  k_init <= node_init;
  k_go <= node_go;
  k_absorb <= node_absorb;
  k_squeeze <= node_squeeze;
  k_din <= node_din;
  node_ready <= k_ready;
  node_dout <= k_dout;

  --main process (fsm based on the original Keccak testbench)
  p_main : process(clk,rst_n)
  begin
    if rst_n = '0' then                 -- asynchronous rst_n (active low)
      node_din <= (others=>'0');
      node_init <= '0';
      node_absorb <= '0';
      node_squeeze <= '0';
      node_go <= '0';
      counter <= 0;
      st <= initial;
      s_node_done <= '0';
      node_out <= (others => '0');
      
    elsif rising_edge(clk) then 
      case st is    -- fsm
        when initial => -- wait for "start" to compute
          if(node_start='1') then
            st <= read_first_input;
            node_init <= '1';
            counter <= 0;
          end if;
          
        when read_first_input =>  -- read the first 64 bits from the left child
          node_init<='0';
          if(counter=0) then
            node_din<=n_l(255 downto 192);
            node_absorb<='1';	
            st<=st0;
            counter <= 1;
          end if;            				

        when st0 =>
          if(counter<9) then  -- loop to read both cildren
            case counter is
              when 1 =>
                node_din <= n_l(191 downto 128);
              when 2 =>
                node_din <= n_l(127 downto 64);
              when 3 =>
                node_din <= n_l(63 downto 0);
              when 4 =>
                node_din <= n_r(255 downto 192);
              when 5 =>
                node_din <= n_r(191 downto 128);
              when 6 =>
                node_din <= n_r(127 downto 64);
              when others =>
                node_din <= n_r(63 downto 0);
            end case;         
            node_absorb <= '1';						
            counter <= counter+1;
            st<=st0;
          else
            st <= st1;
            node_absorb <= '0';
            node_go <= '1';
          end if;
          
        when st1 =>
          node_go <= '0';
          -- wait one clock cycle before checking if the core is ready or not
          st <= st1a;
          
        when st1a =>				
          if(node_ready='0') then
            st <= st1;
          else
            st <= END_HASH1;
          end if;
          
        when END_HASH1 =>
          if(node_ready='1') then
            node_squeeze <= '1';
            st <= END_HASH2;
            counter <= 0;
          end if;
          
        when END_HASH2 =>
          case counter is     -- putting the 64-bit value at the right place in the output register
            when 0 =>
              node_out(255 downto 192) <= node_dout;
            when 1 =>
              node_out(191 downto 128) <= node_dout;
            when 2 =>
              node_out(127 downto 64) <= node_dout;
            when others =>
              node_out(63 downto 0) <= node_dout;
          end case;
          
          if(counter<3) then
            counter <= counter+1;
          else
            node_squeeze <= '0';
            counter <= 0;
            st <= stop_k;
            s_node_done <= '1';
          end if;
          
        when stop_k =>  -- end of the keccak operation
          s_node_done <= '0';
          st <= initial;

        end case;
    end if;
  end process;

end rtl;