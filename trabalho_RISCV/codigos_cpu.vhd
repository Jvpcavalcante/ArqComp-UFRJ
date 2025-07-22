----PC-----

LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY PC IS
  PORT (
    clk    : IN  std_logic;
    reset  : IN  std_logic;
    enable : IN  std_logic;  -- << NOVO SINAL DE HABILITAÇÃO
    pcIn   : IN  std_logic_vector(31 DOWNTO 0);
    pcOut  : OUT std_logic_vector(31 DOWNTO 0)
  );
END PC;

ARCHITECTURE TypeArchitecture OF PC IS
BEGIN
  PROCESS (clk, reset)
  BEGIN
    -- O reset tem prioridade máxima
    IF (reset = '1') THEN
      pcOut <= (others => '0');
    ELSIF (rising_edge(clk)) THEN
      -- Só atualiza o PC se estiver habilitado
      IF (enable = '1') THEN
        pcOut <= pcIn;
      END IF;
      -- Se enable for '0', nada acontece, pcOut mantém seu valor.
    END IF;
  END PROCESS;
END TypeArchitecture;


-----PC plus 4-------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; -- Required for unsigned arithmetic

entity pc_plus_4_adder is
    port (
        pc_in  : in  std_logic_vector(31 downto 0); -- Input: Current Program Counter value
        pc_out : out std_logic_vector(31 downto 0)  -- Output: PC + 4
    );
end pc_plus_4_adder;

architecture Behavioral of pc_plus_4_adder is
begin
    -- Process to perform the addition
    -- This is a combinational process, meaning the output updates immediately
    -- when pc_in changes.
    process (pc_in)
    begin
        -- Convert the input std_logic_vector to an unsigned type for arithmetic operations.
        -- Add 4 to the unsigned PC value.
        -- Convert the result back to std_logic_vector for the output.
        pc_out <= std_logic_vector(unsigned(pc_in) + 4);
    end process;

end Behavioral;

-----IF/ID------

LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY IFID IS
    PORT (
    clk                 : IN  std_logic;
    Instruction_in      : IN  std_logic_vector(31 downto 0);
    PC_plus_4           : IN  std_logic_vector(31 downto 0);
    full_inst_out       : OUT std_logic_vector(31 downto 0);
    enable      : IN  std_logic; -- << NOVO SINAL DE HABILITAÇÃO
    PC_added_out        : OUT std_logic_vector(31 downto 0);
	Rd		            : out STD_LOGIC_VECTOR(4 downto 0);
    Rs1		            : out STD_LOGIC_VECTOR(4 downto 0);
    Rs2		            : out STD_LOGIC_VECTOR(4 downto 0);
	Func3		        : out STD_LOGIC_VECTOR(2 downto 0);
	Func7		        : out STD_LOGIC_VECTOR(6 downto 0);
    opcode_out		    : out STD_LOGIC_VECTOR(6 downto 0)

    );
END IFID;

ARCHITECTURE TypeArchitecture OF IFID IS
    SIGNAL signal_instruction : std_logic_vector(31 downto 0);
    SIGNAL signal_pc_added    : std_logic_vector(31 downto 0);
BEGIN

    PROCESS (clk)
    BEGIN
        IF (rising_edge(clk)) THEN
		IF (enable = '1') THEN
            		signal_instruction <= Instruction_in;
            		signal_pc_added <= PC_plus_4;
		END IF;
        END IF;
        
        IF (falling_edge(clk)) THEN
            -- Lógica para a borda de descida do clock
            full_inst_out <= signal_instruction;
            PC_added_out <= signal_pc_added;
            rs1 <= signal_instruction(19 downto 15);
	    rs2 <= signal_instruction(24 downto 20);
	    rd <= signal_instruction(11 downto 7);
	    func3 <= signal_instruction(14 downto 12);
	    func7 <= signal_instruction(31 downto 25);
        opcode_out <= signal_instruction(6 downto 0);

        END IF;
    END PROCESS;

END TypeArchitecture;

----Control UNIT----

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity Control_Unit is
    port(
        opcode        : IN  std_logic_vector(6 downto 0);
        -- Sinais de Controle para o Datapath
        ALUSrcA       : OUT std_logic; -- NOVO (era 'blockA'): '0'->rs1, '1'->PC para ALU input A
        ALUSrcB        : OUT std_logic; -- '0'->rs2, '1'->imediato para ALU input B
        RegWrite      : OUT std_logic;
        MemRead       : OUT std_logic;
        MemWrite      : OUT std_logic;
        Branch        : OUT std_logic; -- Para beq
        BranchNotEq   : OUT std_logic; -- Para bne
        BrIncond      : OUT std_logic; -- Para jal, jalr
        regToPC       : OUT std_logic; -- Para jalr
        AluOp         : OUT std_logic_vector(1 downto 0);
        WriteBackSrc  : OUT std_logic_vector(1 downto 0) -- RENOMEADO (era 'regSrc')
    );
end Control_Unit;

architecture TypeArchitecture of Control_Unit is
begin
    process(opcode)
    begin
        -- Valores padrão (segurança para evitar latches e definir um estado NOP)
        ALUSrcA      <= '0';
        ALUSrcB       <= '0';
        RegWrite     <= '0';
        MemRead      <= '0';
        MemWrite     <= '0';
        Branch       <= '0';
        BranchNotEq  <= '0';
        BrIncond     <= '0';
        regToPC      <= '0';
        AluOp        <= "00";
        WriteBackSrc <= "00";

        CASE opcode IS
            -- Tipo-R: add, sub, etc.
            WHEN "0110011" =>
                RegWrite     <= '1';
                AluOp        <= "10"; -- ALU faz operação baseada em funct3/7

            -- Tipo-I: addi, slti, etc.
            WHEN "0010011" =>
                ALUSrcB       <= '1'; -- ALU usa imediato
                RegWrite     <= '1';
                AluOp        <= "11"; -- ALU faz operação baseada em funct3

            -- Load: lw
            WHEN "0000011" =>
                ALUSrcB       <= '1'; -- ALU usa imediato (offset)
                RegWrite     <= '1';
                MemRead      <= '1';
                AluOp        <= "00"; -- ALU soma para endereço
                WriteBackSrc <= "01"; -- Dado da memória vai para registrador

            -- Store: sw
            WHEN "0100011" =>
                ALUSrcB       <= '1'; -- ALU usa imediato (offset)
                MemWrite     <= '1';
                AluOp        <= "00"; -- ALU soma para endereço

            -- Branch: beq, bne
            WHEN "1100011" => -- Opcode padrão para BEQ e BNE
                AluOp        <= "01"; -- ALU subtrai para comparar
                -- O sinal específico (Branch ou BranchNotEq) será determinado pelo funct3

            -- LUI (Load Upper Immediate)
            WHEN "0110111" =>
                ALUSrcB       <= '1'; -- ALU usa imediato
                RegWrite     <= '1';
                -- A mágica do LUI é que o imediato já está nos 20 bits superiores.
                -- A ALU pode simplesmente passar o imediato para a saída.
                -- (Primeira entrada da ALU pode ser zero).
		
	-- JALR (Jump and Link Register)
	WHEN "1100111" =>
    		ALUSrcA      <= '0';       -- ALU usa rs1
    		ALUSrcB       <= '1';       -- ALU usa imediato (offset)
    		RegWrite     <= '1';       -- Habilita escrita em rd
    		BrIncond     <= '1';       -- Ativa lógica de salto incondicional no PC_MUX
    		regToPC      <= '1';       -- Sinal específico para indicar que o alvo vem da ALU(rs1+imm)
    		AluOp        <= "00";       -- ALU deve somar
    		WriteBackSrc <= "10";       -- Seleciona PC+4 para ser escrito em rd (Link)

	-- JAL (Jump and Link)
	WHEN "1101111" =>
    		ALUSrcA      <= '1';       -- ALU usa o PC
    		ALUSrcB       <= '1';       -- ALU usa o imediato (offset)
    		RegWrite     <= '1';       -- Habilita escrita em rd
    		BrIncond     <= '1';       -- Ativa lógica de salto incondicional no PC_MUX
    		-- regToPC é '0', pois o alvo é PC+imm (não rs1+imm)
    		WriteBackSrc <= "10";       -- Seleciona PC+4 para ser escrito em rd (Link)

            -- AUIPC (Add Upper Immediate to PC)
            WHEN "0010111" => -- Opcode padrão para AUIPC
                ALUSrcA      <= '1'; -- ALU usa PC como primeira entrada
                ALUSrcB       <= '1'; -- ALU usa imediato como segunda entrada
                RegWrite     <= '1';
                AluOp        <= "00"; -- ALU soma PC + Imediato
                WriteBackSrc <= "00"; -- Resultado da ALU vai para registrador

            -- Outros opcodes não fazem nada (NOP)
            WHEN OTHERS =>
                NULL;

        END CASE;
    end process;
end TypeArchitecture;


-----Register File--------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity register_file is
  port(
    outA         : out std_logic_vector(31 downto 0);
    outB         : out std_logic_vector(31 downto 0);
    input        : in  std_logic_vector(31 downto 0);
    regSelManual : in  std_logic_vector(4 downto 0);
    outRegManual : out std_logic_vector(31 downto 0);
    writeEnable  : in  std_logic;
    regASel      : in  std_logic_vector(4 downto 0);
    regBSel      : in  std_logic_vector(4 downto 0);
    writeRegSel  : in  std_logic_vector(4 downto 0);
    clk          : in  std_logic
  );
end register_file;

architecture TypeArchitecture of register_file is
  type registerFile is array(0 to 31) of std_logic_vector(31 downto 0);
  signal registers : registerFile := (
    "00000000000000000000000000000000", "00000000000000000000000000000000",
    "00000000000000000000000000000000", "00000000000000000000000000000000",
    "00000000000000000000000000000000", "00000000000000000000000000000000",
    "00000000000000000000000000000000", "00000000000000000000000000000000",
    "00000000000000000000000000000000", "00000000000000000000000000000000",
    "00000000000000000000000000000000", "00000000000000000000000000000000",
    "00000000000000000000000000000000", "00000000000000000000000000000000",
    "00000000000000000000000000000000", "00000000000000000000000000000000",
    "00000000000000000000000000000000", "00000000000000000000000000000000",
    "00000000000000000000000000000000", "00000000000000000000000000000000",
    "00000000000000000000000000000000", "00000000000000000000000000000000",
    "00000000000000000000000000000000", "00000000000000000000000000000000",
    "00000000000000000000000000000000", "00000000000000000000000000000000",
    "00000000000000000000000000000000", "00000000000000000000000000000000",
    "00000000000000000000000000000000", "00000000000000000000000000000000",
    "00000000000000000000000000000000", "00000000000000000000000000000000"
  );
  
  begin
  regFile : process (clk) is
    begin
      if rising_edge(clk) then
        if (registers(to_integer(unsigned(regASel)))) = "UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU" THEN
          outA <= (others => '0');
        else
          outA <= registers(to_integer(unsigned(regASel)));
        end if;

        if (registers(to_integer(unsigned(regBSel)))) = "UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU" THEN
          outB <= (others => '0');
        else
          outB <= registers(to_integer(unsigned(regBSel)));
        end if;

        if writeEnable = '1' then
          registers(to_integer(unsigned(writeRegSel))) <= input;
        end if;
    end if;
  end process;

  outRegManual <= registers(to_integer(unsigned(regSelManual)));

end TypeArchitecture;


-----Immmediate-----


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity ImmGen is
    Port ( 
        inst : in std_logic_vector(31 downto 0);
        imm : out std_logic_vector(31 downto 0)
    );
end ImmGen;

architecture Behavioral of ImmGen is
    signal opcode : std_logic_vector(6 downto 0);
    signal imm_interno : std_logic_vector(31 downto 0) := 
                        "00000000000000000000000000000000";
begin
    opcode <= inst(6 downto 0);

    process(opcode)
    begin
        if (opcode = "0010011" or opcode = "1100110") then  -- I type
            imm_interno(11 downto 0) <= inst(31 downto 20);
            imm_interno(31 downto 12) <= (others => inst(31));

        elsif (opcode = "1110111" or opcode = "0110111") then  -- U type
            imm_interno(31 downto 12) <= inst(31 downto 12);
        elsif (opcode = "0100011") then  -- S type
            imm_interno(11 downto 5) <= inst(31 downto 25);
            imm_interno(4 downto 0) <= inst(11 downto 7);
            
        elsif (opcode = "1100111" or opcode = "1100011") then  -- SB type
            imm_interno(12) <= inst(31);
            imm_interno(11) <= inst(7);
            imm_interno(10 downto 5) <= inst(30 downto 25);
            imm_interno(4 downto 1) <= inst(11 downto 8);
            imm_interno(31 downto 13) <= (others => inst(31));
        elsif (opcode = "1101111") then  -- UJ type
            imm_interno(20) <= inst(31);
            imm_interno(19 downto 12) <= inst(19 downto 12);
            imm_interno(11) <= inst(20);
            imm_interno(10 downto 1) <= inst(30 downto 21);
            
        end if;
    end process;

    imm <= imm_interno;

end Behavioral;



-------ID/EX---------


LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY ID_EX_Registro_pipeline IS
    PORT (
        clk             : IN  std_logic;
	PC_IN	       : IN STD_LOGIC_VECTOR(31 downto 0);
        AluSrcA_IN       : IN std_logic;
	AluSrcB_IN       : IN std_logic;
        RegWrite_IN     : IN std_logic;
        MemRead_IN      : IN std_logic;
        MemWrite_IN     : IN std_logic;
        Branch_IN       : IN std_logic;
        AluOp_IN        : IN std_logic_vector(1 downto 0);
        Rd              : IN STD_LOGIC_VECTOR(4 downto 0);
        Rs1             : IN STD_LOGIC_VECTOR(4 downto 0);
        Rs2             : IN STD_LOGIC_VECTOR(4 downto 0);
        Func3           : IN STD_LOGIC_VECTOR(2 downto 0);
        Func7           : IN STD_LOGIC_VECTOR(6 downto 0);
        Read_Data1      : IN STD_LOGIC_VECTOR(31 downto 0);
        Read_Data2      : IN STD_LOGIC_VECTOR(31 downto 0);
        Immediate       : IN STD_LOGIC_VECTOR(31 downto 0);
        zera_sinais     : IN std_logic;
	BranchNotEq_IN   : IN  std_logic; -- << NOVO
        BrIncond_IN      : IN  std_logic; -- << NOVO
        regToPC_IN       : IN  std_logic; -- << NOVO
	WriteBackSrc_IN  : IN  std_logic_vector(1 downto 0); -- << NOVO

	PC_OUT	       : OUT STD_LOGIC_VECTOR(31 downto 0);
        ID_EX_AluSrcA    : OUT std_logic;
	ID_EX_AluSrcB    : OUT std_logic;
        ID_EX_RegWrite  : OUT std_logic;
        ID_EX_MemRead   : OUT std_logic;
        ID_EX_MemWrite  : OUT std_logic;
        ID_EX_Branch    : OUT std_logic;
        ID_EX_AluOp     : OUT std_logic_vector(1 downto 0);
        ID_EX_Rd        : OUT STD_LOGIC_VECTOR(4 downto 0);
        ID_EX_Rs1       : OUT STD_LOGIC_VECTOR(4 downto 0);
        ID_EX_Rs2       : OUT STD_LOGIC_VECTOR(4 downto 0);
        ID_EX_Func3     : OUT STD_LOGIC_VECTOR(2 downto 0);
        ID_EX_Func7     : OUT STD_LOGIC_VECTOR(6 downto 0);
        ID_EX_Read_Data1: OUT STD_LOGIC_VECTOR(31 downto 0);
        ID_EX_Read_Data2: OUT STD_LOGIC_VECTOR(31 downto 0);
        ID_EX_Immediate : OUT STD_LOGIC_VECTOR(31 downto 0);
	ID_EX_BranchNotEq: OUT std_logic; -- << NOVO
        ID_EX_BrIncond   : OUT std_logic; -- << NOVO
        ID_EX_regToPC    : OUT std_logic; -- << NOVO
	ID_EX_WriteBackSrc: OUT std_logic_vector(1 downto 0) -- << NOVO

    );
END ID_EX_Registro_pipeline;

ARCHITECTURE TypeArchitecture OF ID_EX_Registro_pipeline IS
BEGIN
    PROCESS (clk)
    BEGIN
        IF (rising_edge(clk)) THEN
            -- Se zera_sinais for '1', insere uma bolha (NOP) limpa
            IF zera_sinais = '1' THEN
                -- Zera TODOS os sinais de controle, dados e endereços
                PC_OUT             <= (others => '0');
                ID_EX_Read_Data1   <= (others => '0');
                ID_EX_Read_Data2   <= (others => '0');
                ID_EX_Immediate    <= (others => '0');
                ID_EX_Rs1          <= (others => '0');
                ID_EX_Rs2          <= (others => '0');
                ID_EX_Rd           <= (others => '0');
                ID_EX_Func3        <= (others => '0');
                ID_EX_Func7        <= (others => '0');
                ID_EX_AluSrcA      <= '0';
                ID_EX_AluSrcB      <= '0'; -- Renomeie para ID_EX_ALUSrc para consistência
                ID_EX_RegWrite     <= '0';
                ID_EX_MemRead      <= '0';
                ID_EX_MemWrite     <= '0';
                ID_EX_Branch       <= '0';
                ID_EX_BranchNotEq  <= '0';
                ID_EX_BrIncond     <= '0';
                ID_EX_regToPC      <= '0';
                ID_EX_AluOp        <= (others => '0');
                ID_EX_WriteBackSrc <= (others => '0');

            -- Caso contrário, opera normalmente passando os valores
            ELSE
                PC_OUT             <= PC_IN;
                ID_EX_Read_Data1   <= Read_Data1;
                ID_EX_Read_Data2   <= Read_Data2;
                ID_EX_Immediate    <= Immediate;
                ID_EX_Rs1          <= Rs1;
                ID_EX_Rs2          <= Rs2;
                ID_EX_Rd           <= Rd;
                ID_EX_Func3        <= Func3;
                ID_EX_Func7        <= Func7;
                ID_EX_AluSrcA      <= AluSrcA_IN;
                ID_EX_AluSrcB      <= AluSrcB_IN;
                ID_EX_RegWrite     <= RegWrite_IN;
                ID_EX_MemRead      <= MemRead_IN;
                ID_EX_MemWrite     <= MemWrite_IN;
                ID_EX_Branch       <= Branch_IN;
                ID_EX_BranchNotEq  <= BranchNotEq_IN;
                ID_EX_BrIncond     <= BrIncond_IN;
                ID_EX_regToPC      <= regToPC_IN;
                ID_EX_AluOp        <= AluOp_IN;
                ID_EX_WriteBackSrc <= WriteBackSrc_IN;
            END IF;
        END IF;
    END PROCESS;
END TypeArchitecture;



------FOrwARD UNIT--------




library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity Forward is
    port(
        clk                 : IN std_logic;
        regWriteWB, regWriteMEM : IN std_logic;
        RSrc1, RSrc2        : IN std_logic_vector(4 downto 0);
        RDWB, RDMEM         : IN std_logic_vector(4 downto 0);
        forwardA, forwardB  : OUT std_logic_vector(1 downto 0)
    );
end Forward;

architecture TypeArchitecture of Forward is
    signal forwardA_interno : std_logic_vector(1 downto 0) := "00";
    signal forwardB_interno : std_logic_vector(1 downto 0) := "00";
    signal RDMEMzero        : std_logic;
    signal RDWBzero         : std_logic;
begin
    process(clk)
    begin
        if (RDMEM = "00000") then
            RDMEMzero <= '1';
        else
            RDMEMzero <= '0';
        end if;

        if (RDWB = "00000") then
            RDWBzero <= '1';
        else
            RDWBzero <= '0';
        end if;

        if ((regWriteMEM = '1') and (RDMEMzero = '0') and (RDMEM = RSrc1)) then
            forwardA_interno <= "10";
        end if;

        if ((regWriteMEM = '1') and (RDMEMzero = '0') and (RDMEM = RSrc2)) then
            forwardB_interno <= "10";
        end if;

        if ((regWriteWB = '1') and (RDWBzero = '0') and (RDWB = RSrc1)) then
            forwardA_interno <= "01";
        end if;

        if ((regWriteWB = '1') and (RDWBzero = '0') and (RDWB = RSrc2)) then
            forwardB_interno <= "01";
        end if;

    end process;

    forwardA <= forwardA_interno;
    forwardB <= forwardB_interno;
end TypeArchitecture;




--------ALU CONTROL---------




library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity ALU_Control is
    port(
        -- Entradas de controle
        AluOp   : IN  std_logic_vector(1 downto 0);
        funct3  : IN  std_logic_vector(2 downto 0);
        funct7  : IN  std_logic_vector(6 downto 0);
        -- Saída de 4 bits para a ALU
        Control : OUT std_logic_vector(3 downto 0)
    );
end ALU_Control;

architecture TypeArchitecture of ALU_Control is
    -- Mapeamento dos códigos de controle da ALU para referência
    -- "0010" -> ADD
    -- "0110" -> SUB
    -- "0000" -> AND
    -- "0001" -> OR
    -- "0101" -> XOR
    -- "0011" -> SLL
    -- "0111" -> SRL / SRA
begin
    process(AluOp, funct3, funct7)
    begin
        CASE AluOp IS
            -- Caso 1: lw, sw, addi (para jalr/auipc) -> Operação é sempre SOMA
            WHEN "00" =>
                Control <= "0010"; -- ADD

            -- Caso 2: beq, bne -> Operação é sempre SUBTRAÇÃO
            WHEN "01" =>
                Control <= "0110"; -- SUB

            -- Caso 3: Instruções Tipo-R. A operação depende de funct3 e funct7.
            WHEN "10" =>
                CASE funct3 IS
                    -- ADD ou SUB
                    WHEN "000" =>
                        IF funct7(5) = '0' THEN
                            Control <= "0010"; -- ADD
                        ELSE
                            Control <= "0110"; -- SUB
                        END IF;
                    -- SLL
                    WHEN "001" =>
                        Control <= "0011";
                    -- XOR
                    WHEN "100" =>
                        Control <= "0101";
                    -- SRL ou SRA
                    WHEN "101" =>
                        Control <= "0111";
                    -- OR
                    WHEN "110" =>
                        Control <= "0001";
                    -- AND
                    WHEN "111" =>
                        Control <= "0000";
                    -- Outros
                    WHEN OTHERS =>
                        Control <= "0000"; -- Padrão seguro
                END CASE;

            -- Caso 4: Instruções Tipo-I (com imediato). A operação depende apenas de funct3.
            WHEN "11" =>
                CASE funct3 IS
                    -- ADDI
                    WHEN "000" =>
                        Control <= "0010";
                    -- SLLI
                    WHEN "001" =>
                        Control <= "0011";
                    -- XORI
                    WHEN "100" =>
                        Control <= "0101";
                    -- SRLI / SRAI
                    WHEN "101" =>
                        Control <= "0111";
                    -- ORI
                    WHEN "110" =>
                        Control <= "0001";
                    -- ANDI
                    WHEN "111" =>
                        Control <= "0000";
                    -- Outros
                    WHEN OTHERS =>
                        Control <= "0000"; -- Padrão seguro
                END CASE;
            
            -- Padrão de segurança
            WHEN OTHERS =>
                Control <= "0000";

        END CASE;
    end process;
end TypeArchitecture;



------ALU-------



library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

entity ALU is
    port(
        A, B    : in std_logic_vector(31 downto 0);
        control : in std_logic_vector(3 downto 0);
        result  : out std_logic_vector(31 downto 0);
        zero    : out std_logic) ;
end ALU;

architecture TypeArchitecture of ALU is

    signal zero_out : std_logic_vector(32 downto 0);
    
    signal result_out : std_logic_vector(31 downto 0);

begin

    result_out <= (A + B) when control = "0010" else
                  (A - B) when control = "0110" else
                  (A XOR B) when control = "0101" else
                  (A OR B) when control = "0001" else
                  (A AND B) when control = "0000" else
                  (std_logic_vector(shift_left(unsigned(A), to_integer(unsigned(B))))) when control = "0011" else
                  (std_logic_vector(shift_right(unsigned(A), to_integer(unsigned(B))))) when control = "0111";
                  
    result <= result_out;
    zero_out(0) <= '0';
    G2: for I in 1 to 32 generate
        zero_out(I) <= zero_out(I - 1) or result_out(I - 1);
    end generate;
    zero <= not zero_out(32);
end TypeArchitecture;


--------EX/MEM----------


LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY EX_MEM IS
    PORT (
        clk         : IN  std_logic;
        -- Entradas do estágio EX
        ALU_Result  : IN  std_logic_vector(31 downto 0);
        Read_Data2  : IN  std_logic_vector(31 downto 0);
        Rd          : IN  std_logic_vector(4 downto 0);
        MemRead     : IN  std_logic;
        MemWrite    : IN  std_logic;
        RegWrite_IN : IN  std_logic; -- << NOVO SINAL DE ENTRADA
	WriteBackSrc_IN  : IN  std_logic_vector(1 downto 0); -- << NOVO

        -- Saídas para o estágio MEM
        EX_MEM_ALU_Result  : OUT std_logic_vector(31 downto 0);
        EX_MEM_Read_Data2  : OUT std_logic_vector(31 downto 0);
        EX_MEM_Rd          : OUT std_logic_vector(4 downto 0);
        EX_MEM_MemRead     : OUT std_logic;
        EX_MEM_MemWrite    : OUT std_logic;
        EX_MEM_RegWrite    : OUT std_logic;  -- << NOVO SINAL DE SAÍDA
	WriteBackSrc_OUT   : OUT  std_logic_vector(1 downto 0) -- << NOVO
    );
END EX_MEM;

ARCHITECTURE TypeArchitecture OF EX_MEM IS
BEGIN

    PROCESS (clk)
    BEGIN
        IF (rising_edge(clk)) THEN
            -- Transferência dos sinais de entrada para as saídas
            EX_MEM_ALU_Result  <= ALU_Result;
            EX_MEM_Read_Data2  <= Read_Data2;
            EX_MEM_Rd          <= Rd;
            EX_MEM_MemRead     <= MemRead;
            EX_MEM_MemWrite    <= MemWrite;
            EX_MEM_RegWrite    <= RegWrite_IN; -- << NOVA ATRIBUIÇÃO
	    WriteBackSrc_OUT   <= WriteBackSrc_IN;
        END IF;
    END PROCESS;

END TypeArchitecture;




---------RAM-------------




library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL; -- Necessário para to_integer e unsigned

ENTITY RAM IS
    PORT (
        clk       : IN  std_logic;
        -- Sinais de Controle e Dados do Estágio MEM
        addr      : IN  std_logic_vector(31 downto 0); -- Endereço vindo da ALU
        writeData : IN  std_logic_vector(31 downto 0); -- Dado a ser escrito (de rs2)
        memWrite  : IN  std_logic; -- Sinal de habilitação de escrita
        memRead   : IN  std_logic; -- Sinal de habilitação de leitura
        -- Saída de Dados
        readData  : OUT std_logic_vector(31 downto 0) -- Dado lido da memória
    );
END RAM;

ARCHITECTURE Behavioral OF RAM IS
    -- Define uma memória de 1K (1024) palavras de 32 bits
    CONSTANT MEM_SIZE : integer := 1024;
    TYPE memory_array IS ARRAY (0 TO MEM_SIZE - 1) OF std_logic_vector(31 DOWNTO 0);

    -- Cria o sinal da memória. Pode ser inicializado com valores para teste.
    SIGNAL memory : memory_array := (others => (others => '0'));

    -- Sinal interno para o endereço da palavra (10 bits para 1024 posições)
    SIGNAL word_addr : unsigned(9 DOWNTO 0);

BEGIN
    -- Converte o endereço de byte de 32 bits em um endereço de palavra de 10 bits
    -- Dividir por 4 é o mesmo que pegar os bits a partir do bit 2.
    -- Usamos os bits [11:2] para ter um endereço de palavra de 10 bits.
    word_addr <= unsigned(addr(11 DOWNTO 2));

    -- LÓGICA DE ESCRITA (SÍNCRONA)
    PROCESS (clk)
    BEGIN
        IF (rising_edge(clk)) THEN
            -- Escreve na memória somente se memWrite estiver ativo
            IF (memWrite = '1') THEN
                memory(to_integer(word_addr)) <= writeData;
            END IF;
        END IF;
    END PROCESS;

    -- LÓGICA DE LEITURA (ASSÍNCRONA/COMBINACIONAL)
    -- O dado lido é disponibilizado na saída se memRead estiver ativo.
    -- Caso contrário, a saída é zero para evitar leituras indesejadas.
    readData <= memory(to_integer(word_addr)) WHEN memRead = '1' ELSE
                (others => '0');

END Behavioral;


-------MEM/WB----------



LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY MEM_WB IS
    PORT (
        clk                : IN  std_logic;
        -- Entradas do estágio MEM
        ALU_Result_IN      : IN  std_logic_vector(31 downto 0);
        Mem_Read_Data_IN   : IN  std_logic_vector(31 downto 0);
        Rd_IN              : IN  std_logic_vector(4 downto 0);
        RegWrite_IN        : IN  std_logic;
	WriteBackSrc_IN    : IN  std_logic_vector(1 downto 0);

        -- Saídas para o estágio WB (e para a Unidade de Forward)
        MEM_WB_ALU_Result    : OUT std_logic_vector(31 downto 0);
        MEM_WB_Mem_Read_Data : OUT std_logic_vector(31 downto 0);
        MEM_WB_Rd            : OUT std_logic_vector(4 downto 0);
        MEM_WB_RegWrite      : OUT std_logic;
	WriteBackSrc_OUT   : OUT  std_logic_vector(1 downto 0)
    );
END MEM_WB;

ARCHITECTURE TypeArchitecture OF MEM_WB IS
BEGIN

    PROCESS (clk)
    BEGIN
        IF (rising_edge(clk)) THEN
            -- Simplesmente registra os valores de entrada nas saídas
            MEM_WB_ALU_Result    <= ALU_Result_IN;
            MEM_WB_Mem_Read_Data <= Mem_Read_Data_IN;
            MEM_WB_Rd            <= Rd_IN;
            MEM_WB_RegWrite      <= RegWrite_IN;
	    WriteBackSrc_OUT     <= WriteBackSrc_IN;
        END IF;
    END PROCESS;

END TypeArchitecture;