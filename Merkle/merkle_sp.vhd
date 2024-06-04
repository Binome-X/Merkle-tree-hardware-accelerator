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

entity merkle_sp is
  generic(
    s_width     : integer := 32;    -- serial word length
    p_width     : integer := 256;   -- parallel word length
    n_leaves    : integer := 8      -- input leaves number
  );
  port(
    clk         : in  std_logic;
    rst_n       : in  std_logic;
    l_go        : in  std_logic;        -- start input
    finished    : out std_logic := '0'; -- output finished (1 cycle only)
    s_in        : in  std_logic_vector(s_width-1 downto 0);
    p_out       : out std_logic_vector(n_leaves*p_width-1 downto 0) := (others => '0');
    in_ok				: in  std_logic
  );
end entity merkle_sp;

architecture rtl of merkle_sp is
  signal tmp : std_logic_vector(n_leaves*p_width-1 downto 0) := (others => '0');  -- temporary computed parallel word
  signal n_l : integer range 0 to n_leaves-1 := 0;        -- number of current leaf
  signal n_w : integer range 0 to p_width/s_width-1 := 0; -- number of current word in the leaf
  signal s_finished : std_logic;    -- one cycle to tell the output is valid
  signal s_r_finished : std_logic;  -- nothing should happen anymore
  
begin
  proc_out: process(clk, rst_n, l_go, s_finished, in_ok)
  begin
    if rst_n = '0' then
      tmp <= (others => '0');
      n_l <= 0;
      n_w <= 0;
      s_finished <= '0';
      s_r_finished <= '0';
    elsif rising_edge(clk) then
      if s_finished='1' then
        s_finished <= '0';
      end if;

      if l_go='1' and in_ok='1' then
        if n_w < p_width/s_width-1 then
          n_w <= n_w+1;
        else
          n_w <= 0;
          if n_l < n_leaves-1 then
            n_l <= n_l + 1;
          else
            if s_r_finished = '0' then
              s_finished <= '1';
              s_r_finished <= '1';
            end if;
          end if;
        end if;
        
        if s_r_finished = '0' then
          tmp(n_leaves*p_width-1 downto s_width) <= tmp(n_leaves*p_width-1-s_width downto 0);
          tmp(s_width-1 downto 0) <= s_in;    
        end if;  
      end if;
    end if;
  end process;
  
  p_out <= tmp;
  finished <= s_finished;
  
end rtl;