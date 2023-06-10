--------------------------------------------------------------------------------
--
-- LAB #6 - Processor 
--
--------------------------------------------------------------------------------
Library ieee;
Use ieee.std_logic_1164.all;
Use ieee.numeric_std.all;
Use ieee.std_logic_unsigned.all;

entity bitstorage is
	port(bitin: in std_logic;
		enout: in std_logic;
		writein: in std_logic;
		bitout: out std_logic);
end entity bitstorage;

architecture memlike of bitstorage is
	signal q: std_logic := '0';
begin
	process(writein) is
	begin
		if (rising_edge(writein)) then
			q <= bitin;
		end if;
	end process;
	
	-- Note that data is output only when enout = 0	
	bitout <= q when enout = '0' else 'Z';
end architecture memlike;

--------------------------------------------------------------------------------
Library ieee;
Use ieee.std_logic_1164.all;
Use ieee.numeric_std.all;
Use ieee.std_logic_unsigned.all;

entity fulladder is
	port (a : in std_logic;
		b : in std_logic;
		cin : in std_logic;
		sum : out std_logic;
		carry : out std_logic
		);
end fulladder;

architecture addlike of fulladder is
begin
	sum   <= a xor b xor cin; 
	carry <= (a and b) or (a and cin) or (b and cin); 
end architecture addlike;


--------------------------------------------------------------------------------
Library ieee;
Use ieee.std_logic_1164.all;
Use ieee.numeric_std.all;
Use ieee.std_logic_unsigned.all;

entity register8 is
	port(datain: in std_logic_vector(7 downto 0);
		enout:  in std_logic;
		writein: in std_logic;
		dataout: out std_logic_vector(7 downto 0));
end entity register8;

architecture memmy of register8 is
	component bitstorage
		port(bitin: in std_logic;
			enout: in std_logic;
			writein: in std_logic;
			bitout: out std_logic);
	end component;
begin
	U0: bitstorage port map(datain(0), enout, writein, dataout(0));
	U1: bitstorage port map(datain(1), enout, writein, dataout(1));
	U2: bitstorage port map(datain(2), enout, writein, dataout(2));
	U3: bitstorage port map(datain(3), enout, writein, dataout(3));
	U4: bitstorage port map(datain(4), enout, writein, dataout(4));	
	U5: bitstorage port map(datain(5), enout, writein, dataout(5));
	U6: bitstorage port map(datain(6), enout, writein, dataout(6));	
	U7: bitstorage port map(datain(7), enout, writein, dataout(7));
	
end architecture memmy;

--------------------------------------------------------------------------------
Library ieee;
Use ieee.std_logic_1164.all;
Use ieee.numeric_std.all;
Use ieee.std_logic_unsigned.all;

entity register32 is
	port(datain: in std_logic_vector(31 downto 0);
		enout32,enout16,enout8: in std_logic;
		writein32, writein16, writein8: in std_logic;
		dataout: out std_logic_vector(31 downto 0));
end entity register32;

architecture biggermem of register32 is
	component register8
		port(datain: in std_logic_vector(7 downto 0);
	     		enout:  in std_logic;
	     		writein: in std_logic;
	     		dataout: out std_logic_vector(7 downto 0));
	end component;

	signal enouts: std_logic_vector(3 downto 0):= "0000";
	signal writins: std_logic_vector(3 downto 0) := "0000";
	-- hint: you'll want to put register8 as a component here 
	-- so you can use it below
begin
	enouts <= (others => '0') 	when enout32 = '0' else
		(3 downto 2 => '1', others => '0')
	when enout16 = '0' else
		(3 downto 1 => '1', others => '0')
	when enout8 = '0' else
		(others => '1');
	writins <= (others => '1')
	when writein32 = '1' else
		(3 downto 2 => '0', others => '1')
	when writein16 = '1' else
		(3 downto 1 => '0', others => '1')
	when writein8 = '1' else
		(others => '0');
	reg32: for i in 4 downto 1 generate
		regi: register8 port map (datain((i*8-1) downto ((i-1)*8)), enouts(i-1), writins(i-1), dataout((i*8-1) downto ((i-1)*8)));

	end generate;
	-- insert code here.
end architecture biggermem;    
--------------------------------------------------------------------------------
LIBRARY ieee;
Use ieee.std_logic_1164.all;
Use ieee.numeric_std.all;
Use ieee.std_logic_unsigned.all;

entity RAM is
    Port(Reset:	  in std_logic;
	 Clock:	  in std_logic;	 
	 OE:      in std_logic;
	 WE:      in std_logic;
	 Address: in std_logic_vector(29 downto 0);
	 DataIn:  in std_logic_vector(31 downto 0);
	 DataOut: out std_logic_vector(31 downto 0));
end entity RAM;

architecture staticRAM of RAM is

   type ram_type is array (0 to 127) of std_logic_vector(31 downto 0);
   signal i_ram : ram_type;

begin

  RamProc: process(Clock, Reset, OE, WE, Address) is

  begin
    if Reset = '1' then
      for i in 0 to 127 loop   
          i_ram(i) <= X"00000000";
      end loop;
    end if;

    if falling_edge(Clock) and WE = '1' and (to_integer(unsigned(Address(7 downto 0))) >= 0)
		and (to_integer(unsigned(Address)) <= 127) then
		i_ram(to_integer(unsigned(Address(7 downto 0)))) <= DataIn;
	-- Add code to write data to RAM
	-- Use to_integer(unsigned(Address)) to index the i_ram array
	
    end if;
		if(OE = '0') and (to_integer(unsigned(Address)) < 127) and (to_integer(unsigned(Address)) >= 0) then
			DataOut <= i_ram(to_integer(unsigned(Address(7 downto 0))));
		else
			DataOut <= (others => 'Z');
	end if;

	-- Rest of the RAM implementation

  end process RamProc;

end staticRAM;	


--------------------------------------------------------------------------------
LIBRARY ieee;
Use ieee.std_logic_1164.all;
Use ieee.numeric_std.all;
Use ieee.std_logic_unsigned.all;

entity Registers is
    Port(ReadReg1: in std_logic_vector(4 downto 0); 
         ReadReg2: in std_logic_vector(4 downto 0); 
         WriteReg: in std_logic_vector(4 downto 0);
	 WriteData: in std_logic_vector(31 downto 0);
	 WriteCmd: in std_logic;
	 ReadData1: out std_logic_vector(31 downto 0);
	 ReadData2: out std_logic_vector(31 downto 0));
end entity Registers;

architecture remember of Registers is
	component register32
  	    port(datain: in std_logic_vector(31 downto 0);
		 enout32,enout16,enout8: in std_logic;
		 writein32, writein16, writein8: in std_logic;
		 dataout: out std_logic_vector(31 downto 0));
	end component;
	
SIGNAL regWrite: std_logic_vector(31 downto 1);
TYPE dataArray is Array(31 downto 1) of std_logic_vector(31 downto 0);
SIGNAL regData: dataArray := (OTHERS=>(OTHERS=>'0'));
	
begin
	regWrite <= (1 => '1', OTHERS => '0') WHEN WriteReg = "00001" AND WriteCmd = '1' ELSE
			(2 => '1', OTHERS => '0') WHEN WriteReg = "00010" AND WriteCmd = '1' ELSE
			(3 => '1', OTHERS => '0') WHEN WriteReg = "00011" AND WriteCmd = '1' ELSE
			(4 => '1', OTHERS => '0') WHEN WriteReg = "00100" AND WriteCmd = '1' ELSE
			(5 => '1', OTHERS => '0') WHEN WriteReg = "00101" AND WriteCmd = '1' ELSE
			(6 => '1', OTHERS => '0') WHEN WriteReg = "00110" AND WriteCmd = '1' ELSE
			(7 => '1', OTHERS => '0') WHEN WriteReg = "00111" AND WriteCmd = '1' ELSE
			(8 => '1', OTHERS => '0') WHEN WriteReg = "01000" AND WriteCmd = '1' ELSE
			(9 => '1', OTHERS => '0') WHEN WriteReg = "01001" AND WriteCmd = '1' ELSE
		    (10 => '1', OTHERS => '0') WHEN WriteReg ="01010" AND WriteCmd = '1' ELSE
		    (11 => '1', OTHERS => '0') WHEN WriteReg ="01011" AND WriteCmd = '1' ELSE
	  	    (12 => '1', OTHERS => '0') WHEN WriteReg ="01100" AND WriteCmd = '1' ELSE
		    (13 => '1', OTHERS => '0') WHEN WriteReg ="01101" AND WriteCmd = '1' ELSE
		    (14 => '1', OTHERS => '0') WHEN WriteReg = "01110" AND WriteCmd = '1' ELSE
		    (15 => '1', OTHERS => '0') WHEN WriteReg ="01111" AND WriteCmd = '1' ELSE
		    (16 => '1', OTHERS => '0') WHEN WriteReg ="10000" AND WriteCmd = '1' ELSE
		(17 => '1', OTHERS => '0') WHEN WriteReg ="10001" AND WriteCmd = '1' ELSE
		(18 => '1', OTHERS => '0') WHEN WriteReg ="10010" AND WriteCmd = '1' ELSE


		    (OTHERS => '0');
	
	
	RegisterMap: FOR i in 31 downto 1 GENERATE
		xi: register32 PORT MAP(WriteData, '0', '1', '1', regWrite(i), '0', '0', regData(i));
	END GENERATE;
		
	WITH ReadReg1 SELECT
		ReadData1 <= regData(1) WHEN "00001",
				regData(2) WHEN "00010",
				regData(3) WHEN "00011",
				regData(4) WHEN "00100",
				regData(5) WHEN "00101",
				regData(6) WHEN "00110",
				regData(7) WHEN "00111",
				regData(8) WHEN "01000",
				regData(9) WHEN "01001",
				regData(10) WHEN "01010",
				regData(11) WHEN "01011",
				regData(12) WHEN "01100",
				regData(13) WHEN "01101",
				regData(14) WHEN "01110",
				regData(15) WHEN "01111",
				regData(16) WHEN "10000",
				regData(17) WHEN "10001",
				regData(18) WHEN "10010",
			

			    (OTHERS => '0') WHEN OTHERS;

	WITH ReadReg2 SELECT
		ReadData2 <= regData(1) WHEN "00001",
					regData(2) WHEN "00010",
					regData(3) WHEN "00011",
					regData(4) WHEN "00100",
					regData(5) WHEN "00101",
					regData(6) WHEN "00110",
					regData(7) WHEN "00111",
					regData(8) WHEN "01000",
					regData(9) WHEN "01001",
					regData(10) WHEN "01010",
					regData(11) WHEN "01011",
					regData(12) WHEN "01100",
					regData(13) WHEN "01101",
					regData(14) WHEN "01110",
					regData(15) WHEN "01111",
					regData(16) WHEN "10000",
					regData(17) WHEN "10001",
					regData(18) WHEN "10010",

					(OTHERS => '0') WHEN OTHERS;
    -- Add your code here for the Register Bank implementation

end remember;
--------------------------------------------------------------------------------
Library ieee;  --GOOD--
Use ieee.std_logic_1164.all;
Use ieee.numeric_std.all;
Use ieee.std_logic_unsigned.all;

entity adder_subtracter is
	port(	datain_a: in std_logic_vector(31 downto 0);
		datain_b: in std_logic_vector(31 downto 0);
		add_sub: in std_logic;
		dataout: out std_logic_vector(31 downto 0);
		co: out std_logic);
end entity adder_subtracter;

architecture calc of adder_subtracter is  
	signal carries: std_logic_vector(31 downto 0);
	signal b_not_b: std_logic_vector(31 downto 0);

	component fulladder
		port (a : in std_logic;
			b : in std_logic;
			cin : in std_logic;
			sum : out std_logic;
			carry : out std_logic
			);
	end component;
	
begin
	b_not_b <= datain_b when add_sub = '0' else NOT datain_b;
	F0: fulladder PORT MAP ( datain_a(0), b_not_b(0), add_sub, dataout(0),carries(0));
	generate_add_sub: FOR i IN 1 to 31 GENERATE
		F: fulladder PORT MAP ( datain_a(i), b_not_b(i), carries(i-1), dataout(i),carries(i));
		end generate;
	-- insert code here.
end architecture calc;

--------------------------------------------------------------------------------
Library ieee;
Use ieee.std_logic_1164.all;
Use ieee.numeric_std.all;
Use ieee.std_logic_unsigned.all;

entity shift_register is
	port(	datain: in std_logic_vector(31 downto 0);
	   	dir: in std_logic;
		shamt:	in std_logic_vector(4 downto 0);
		dataout: out std_logic_vector(31 downto 0));
end entity shift_register;

architecture shifter of shift_register is
	
begin
		dataout <= (datain(30 downto 0) & '0') when (dir = '0' and shamt = "00001") else
			(datain(29 downto 0) & "00") when (dir = '0' and shamt = "00010") else
			(datain(28 downto 0) & "000") when (dir = '0' and shamt = "00011") else
			('0' & datain(31 downto 1)) when (dir = '1' and shamt = "00001") else 
			("00" & datain(31 downto 2)) when (dir = '1' and shamt = "00010") else
			("000" & datain(31 downto 3)) when (dir = '1' and shamt = "00011") else 
			datain;
end architecture shifter;


--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ALU is
  port (
    DataIn1: in std_logic_vector(31 downto 0);
    DataIn2: in std_logic_vector(31 downto 0);
    ALUCtrl: in std_logic_vector(4 downto 0);
    Zero: out std_logic;
    ALUResult: out std_logic_vector(31 downto 0)
  );
end entity ALU;

architecture behavioral of ALU is
  component adder_subtracter is
    port (
      datain_a: in std_logic_vector(31 downto 0);
      datain_b: in std_logic_vector(31 downto 0);
      add_sub: in std_logic;
      dataout: out std_logic_vector(31 downto 0);
      co: out std_logic
    );
  end component adder_subtracter;

  component shift_register is
    port (
      datain: in std_logic_vector(31 downto 0);
      dir: in std_logic;
      shamt: in std_logic_vector(4 downto 0);
      dataout: out std_logic_vector(31 downto 0)
    );
  end component shift_register;

  signal tempResult: std_logic_vector(31 downto 0);
  signal addSubResult: std_logic_vector(31 downto 0);
  signal shiftResult: std_logic_vector(31 downto 0);

begin
  -- Perform addition/subtraction using the adder/subtracter component
  addSubComp: adder_subtracter port map (
    datain_a => DataIn1,
    datain_b => DataIn2,
    add_sub => ALUCtrl(4),
    dataout => addSubResult,
    co => open
  );

  -- Perform shift operation using the shift_register component
  shiftComp: shift_register port map (
    datain => DataIn1,
    dir => ALUCtrl(4),
    shamt => DataIn2(4 downto 0),
    dataout => shiftResult
  );

  -- ALU control signal decoding and output assignment
  with ALUCtrl select
    tempResult <=
      DataIn2 when "00000",
      addSubResult when "00001" | "10010"|"00011",
      DataIn1 and DataIn2 when "00100" | "00101",
      DataIn1 or DataIn2 when "00110"|"00111",
      shiftResult when "01000" | "11000",
	  shiftResult when "01100" | "11100",
      (others => 'X') when others;

  -- Assign Zero output
  Zero <= '1' when tempResult = "00000000000000000000000000000000" else '0';

  -- Assign ALUResult output
  ALUResult <= tempResult;

end architecture behavioral;
--------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity Processor is
    Port ( reset : in  std_logic;
	   clock : in  std_logic);
end Processor;

architecture holistic of Processor is
	component Control
   	     Port( clk : in  STD_LOGIC;
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
	end component;

	component ALU
		Port(DataIn1: in std_logic_vector(31 downto 0);
		     DataIn2: in std_logic_vector(31 downto 0);
		     ALUCtrl: in std_logic_vector(4 downto 0);
		     Zero: out std_logic;
		     ALUResult: out std_logic_vector(31 downto 0) );
	end component;
	
	component Registers
	    Port(ReadReg1: in std_logic_vector(4 downto 0); 
                 ReadReg2: in std_logic_vector(4 downto 0); 
                 WriteReg: in std_logic_vector(4 downto 0);
		 WriteData: in std_logic_vector(31 downto 0);
		 WriteCmd: in std_logic;
		 ReadData1: out std_logic_vector(31 downto 0);
		 ReadData2: out std_logic_vector(31 downto 0));
	end component;

	component InstructionRAM
    	    Port(Reset:	  in std_logic;
		 Clock:	  in std_logic;
		 Address: in std_logic_vector(29 downto 0);
		 DataOut: out std_logic_vector(31 downto 0));
	end component;

	component RAM 
	    Port(Reset:	  in std_logic;
		 Clock:	  in std_logic;	 
		 OE:      in std_logic;
		 WE:      in std_logic;
		 Address: in std_logic_vector(29 downto 0);
		 DataIn:  in std_logic_vector(31 downto 0);
		 DataOut: out std_logic_vector(31 downto 0));
	end component;
	
	component BusMux2to1
		Port(selector: in std_logic;
		     In0, In1: in std_logic_vector(31 downto 0);
		     Result: out std_logic_vector(31 downto 0) );
	end component;
	
	component ProgramCounter
	    Port(Reset: in std_logic;
		 Clock: in std_logic;
		 PCin: in std_logic_vector(31 downto 0);
		 PCout: out std_logic_vector(31 downto 0));
	end component;

	component adder_subtracter
		port(	datain_a: in std_logic_vector(31 downto 0);
			datain_b: in std_logic_vector(31 downto 0);
			add_sub: in std_logic;
			dataout: out std_logic_vector(31 downto 0);
			co: out std_logic);
	end component adder_subtracter;	
	
	SIGNAL PC_adder_to_Mux, Branch_adder_to_Mux: std_logic_vector(31 downto 0);
	SIGNAL PC_adder_co, Branch_adder_co, DataOffset_co: std_logic;
	SIGNAL PCin, PCout: std_logic_vector(31 downto 0);
	SIGNAL ImmGenOut: std_logic_vector(31 downto 0);
	SIGNAL InstructionOut: std_logic_vector(31 downto 0);
	SIGNAL BranchMuxSelect: std_logic := '0';
	SIGNAL Zero: std_logic;
	SIGNAL Branch, ImmGen: std_logic_vector(1 downto 0);
	SIGNAL ALUCtrl: std_logic_vector(4 downto 0);
	SIGNAL MemRead, MemToReg, MemWrite, ALUSrc, RegWrite: std_logic;
	SIGNAL WriteData: std_logic_vector(31 downto 0);
	SIGNAL ALUInA, ALUInB, RegFileOutB, ALUResult: std_logic_vector(31 downto 0);
	SIGNAL DataMemOut: std_logic_vector(31 downto 0);
	SIGNAL ReadReg1, ReadReg2, DestReg: std_logic_vector(4 downto 0);
	SIGNAL OffsetDataAddress: std_logic_vector(31 downto 0);

begin
	
		ReadReg1 <= InstructionOut(19 downto 15);
	ReadReg2 <= InstructionOut(24 downto 20);
	DestReg <= InstructionOut(11 downto 7);


	PC: ProgramCounter PORT MAP(reset, clock, PCin, PCout);
	PC_adder: adder_subtracter PORT MAP(PCout, x"00000004", '0', PC_adder_to_Mux, PC_adder_co);
	Branch_adder: adder_subtracter PORT MAP(PCout, ImmGenOut, '0', Branch_adder_to_Mux, Branch_adder_co);
	PCMux: BusMux2to1 PORT MAP(BranchMuxSelect, PC_adder_to_Mux, Branch_adder_to_Mux, PCin);
	IMEM: InstructionRAM PORT MAP(reset, clock, PCout(31 downto 2), InstructionOut);
	ControlBlock: Control PORT MAP(clock, InstructionOut(6 downto 0), InstructionOut(14 downto 12), InstructionOut(31 downto 25), Branch, MemRead, MemtoReg, ALUCtrl, MemWrite, ALUSrc, RegWrite, ImmGen);
	RegFile: Registers PORT MAP(ReadReg1, ReadReg2, DestReg, WriteData, RegWrite, ALUInA, RegFileOutB);
	ALUInputMux: BusMux2to1 PORT MAP(ALUSrc, RegFileOutB, ImmGenOut, ALUInB);
	MainALU: ALU PORT MAP(ALUInA, ALUInB, ALUCtrl, Zero, ALUResult);
	DMEM: RAM PORT MAP(reset, clock, MemRead, MemWrite, OffsetDataAddress(31 downto 2), RegFileOutB, DataMemOut);
	ALUOutMux: BusMux2to1 PORT MAP(MemToReg, ALUResult, DataMemOut, WriteData);

	DataOffset: adder_subtracter PORT MAP(ALUResult, x"10000000", '1', OffsetDataAddress, DataOffset_co);

	ImmGenOut(31 downto 12) <= (Others=>InstructionOut(31)) WHEN ImmGen = "00" ELSE
				   (Others=>InstructionOut(31)) WHEN ImmGen = "01" ELSE
				   (Others=>InstructionOut(31)) WHEN ImmGen = "10" ELSE
				   InstructionOut(31 downto 12);
	ImmGenOut(11 Downto 0) <= InstructionOut(31 downto 20) WHEN ImmGen = "00" ELSE
				  InstructionOut(31 downto 25) & InstructionOut(11 downto 7) WHEN ImmGen = "01" ELSE
				  InstructionOut(7) & InstructionOut(30 downto 25) & InstructionOut(11 downto 8) & '0' WHEN ImmGen = "10" ELSE
				  (OTHERS=>'0');

	BranchMuxSelect <= '1' WHEN (Branch = "01" AND Zero = '1') OR (Branch = "10" AND Zero = '0') ELSE
			   '0';

	-- Add your code here
end holistic;

