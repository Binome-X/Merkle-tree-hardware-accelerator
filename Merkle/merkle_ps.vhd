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

entity merkle_ps is
  generic(
    s_width     : integer := 32;  -- width of the serial output port
    p_width     : integer := 256; -- width of the parallel input port
    n_out       : integer := 8;    -- hash values number
    logp        : integer := 3;
    logn        : integer := 3
  );
  port(
    clk         : in  std_logic;
    rst_n       : in  std_logic;
    out_go      : in  std_logic;        -- input to tell the component to start
    out_ok      : out std_logic := '0'; -- output signal to tell it's finished
    out_part_idx: out std_logic_vector(logp-1 downto 0);
    p_in        : in  std_logic_vector(p_width-1 downto 0);   -- parallel input
    s_out       : out std_logic_vector(s_width-1 downto 0) := (others => '0')   -- serial output
  );
end entity merkle_ps;

architecture rtl of merkle_ps is
  signal r_out_go : std_logic;  -- register to know the working of the component
  signal n : integer := 0;           -- wich output word is being computed
  signal r_p_in : std_logic_vector(p_width-1 downto 0);
  signal r_out_ok : std_logic;
  signal s2_out_ok : std_logic;
  signal n_node : integer range 0 to 2**logn-1;
begin
  proc_out : process(clk, rst_n, r_out_go, n)
  begin
    if rst_n = '0' then
      n <= 0;
      s_out <= (others => '0');
      r_out_ok <= '0';
      s2_out_ok <= '0';
      n_node <= 0;
    elsif rising_edge(clk) then
      if r_out_go='1' then
        s_out <= p_in(p_width-1-n*s_width downto p_width-(n+1)*s_width);  -- computing an output word
        s2_out_ok <= '1';
        if n<p_width/s_width-1 then
          if n_node<2**logn then
            n <= n+1;
          end if;
        else
          n <= 0;
          if n_node<2**logn-2 then
            n_node <= n_node+1;
          else
            s2_out_ok <= '0';
          end if;
          
          
        end if;
      end if;
      r_out_ok <= s2_out_ok;
    end if;

  end process;
  
  p_go: process(clk, rst_n, out_go) -- register process
  begin
    if rst_n = '0' then
      r_out_go <= '0';
      out_part_idx <= (others => '0');
    elsif rising_edge(clk) then
      if out_go='1' then
        r_out_go <= '1';
      end if;
      if r_out_go='1' and n=p_width/s_width-1 and n_node=2**logn-2 then
        r_out_go <= '0';
      end if;
      out_part_idx <= std_logic_vector(to_unsigned(n, out_part_idx'length));
    end if;
  end process;

  out_ok <= s2_out_ok or r_out_ok;
  
end rtl;