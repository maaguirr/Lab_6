--------------------------------------------------------------------------------
--
-- LAB #6 - Processor Elements
--
--------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity BusMux2to1 is
	Port(	selector: in std_logic;
			In0, In1: in std_logic_vector(31 downto 0);
			Result: out std_logic_vector(31 downto 0) );
end entity BusMux2to1;

architecture selection of BusMux2to1 is
begin
	with selector select
		Result <= In0 when '0',
			In1 when '1',
			(others=>'Z') when others;
-- Add your code here
end architecture selection;

--------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity Control is
      Port(clk : in  STD_LOGIC;
           opcode : in  STD_LOGIC_VECTOR (6 downto 0);
           funct3  : in  STD_LOGIC_VECTOR (2 downto 0);
           funct7  : in  STD_LOGIC_VECTOR (6 downto 0);
           Branch : out  STD_LOGIC_VECTOR(1 downto 0);
           MemRead : out  STD_LOGIC;
           MemtoReg : out  STD_LOGIC;
           ALUCtrl : out  STD_LOGIC_VECTOR(4 downto 0);
           MemWrite : out  STD_LOGIC;
           ALUSrc : out  STD_LOGIC;
           RegWrite : out  STD_LOGIC;
           ImmGen : out STD_LOGIC_VECTOR(1 downto 0));
end Control;

architecture Boss of Control is	

	signal ALUCtrlSelect: std_logic_vector(4 downto 0);

begin
	
	ALUCtrlSelect <= "00001" when opcode = "0110011" and funct3 = "000" and funct7 = "0000000" else --add
		"10010" when opcode = "0110011" and funct3 = "000" and funct7 = "0100000" else --sub
		"00001" when opcode = "0110011" and funct3 = "110" and funct7 = "0000000" else --or
		"00100" when opcode = "0110011" and funct3 = "111" and funct7 = "0000000" else --and
		"01000" when opcode = "0110011" and funct3 = "001" and funct7 = "0000000" else --sll
		"11000" when opcode = "0110011" and funct3 = "101" and funct7 = "0000000" else --srl
		"00011" when opcode = "0010011" and funct3 = "000" else --addi
		"00111" when opcode = "0010011" and funct3 = "110" else --ori
		"00101" when opcode = "0010011" and funct3 = "111" else --andi
		"01100" when opcode = "0010011" and funct3 = "001" else --slli
		"11100" when opcode = "0010011" and funct3 = "101" else --srli
		"00110" when opcode = "1100011" and (funct3= "000" or funct3 = "001") else --bne/beq
		"11010" when opcode = "0000011" or opcode = "0100011" else --lw/sw 
		"11111" when opcode = "0110111" else --lui
		"00000" ; --pass
	ALUCtrl <= ALUCtrlSelect;

	Branch <= "01" when opcode = "1100011" and funct3 = "000" else --beq
		"10" when opcode = "1100011" and funct3 = "001" else --bne
		"00"; --pass
	ALUSrc <= '0' when ALUCtrlSelect(4) = '0' else
		'1';
	ImmGen <= "00" when opcode = "0010011" or opcode = "0000011" else
		"01" when opcode = "0100011" else 
		"10"  when opcode = "1100011" else
		"11";

	RegWrite <= '1' when (opcode = "0110111" or opcode = "0000011" or opcode = "0010011" or opcode = "0110011") and clk = '0' else
		'0';
	MemWrite <= '1' when opcode = "0100011" else
		'0';
	MemRead <= '0' when opcode = "0000011" else
		'1';
	MemtoReg <= '1' when opcode = "0000011" else
		'0';

-- Add your code here

end Boss;

--------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity ProgramCounter is
    Port(Reset: in std_logic;
	 Clock: in std_logic;
	 PCin: in std_logic_vector(31 downto 0);
	 PCout: out std_logic_vector(31 downto 0));
end entity ProgramCounter;

architecture executive of ProgramCounter is
begin
	
	process(Reset, Clock) is
	begin
		if(Reset = '1') then
			PCout <= x"00400000";
		end if;
	if rising_edge(Clock) then
		PCout <= PCin;
	end if;
	end process;
	
-- Add your code here

end executive;
--------------------------------------------------------------------------------
