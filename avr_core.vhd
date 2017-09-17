
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity avr_core is
	port(
		reset_n : in std_logic;
		clock : in std_logic;
		out_addr_bus : out std_logic_vector(15 downto 0);
		inout_data_bus : inout std_logic_vector(7 downto 0);
	);
end avr_core;

architecture logic of avr_core is
	type regarray is array (0 to 31) of (std_logic_vector(7 downto 0)); --
	type exec_c is array (0 to 3) of (integer range 0 to 3);
	--	type instructs is array (add,adc,adiw,sub,subi,sbc,sbci,sbiw,_and,andi,_or,ori,eor,com,neg,sbr,cbr,inc,dec,tst,clr,ser,mul,
	--									rjmp,ijmp,rcall,call,ret,reti,cpse,cp,cpc,cpi,sbrc,sbrs,sbic,sbis,brbs,brbc,breq,brne,brcs,brcc,brsh,brlo,brmi,brpl,brge,brlt,brhs,brhc,brts,brtc,brvs,brvc,brie,brid,
	--									mov,ldi,lds,ld,ldd,sts,st,_std,lpm,_in,_out,push,pop,
		--								lsl,lsr,_rol,_ror,asr,swap,bset,bclr,sbi,cbi,bst,bld,sec,clc,sen,cln,sez,clz,sei,cli,ses,cls,sev,clv,set,clt,seh,clh,nop,sleep,wdr);
	signal regs : regarray;
	signal immidiate : std_logic_vector(7 downto 0);
	signal reg_16buf : std_logic_vector(7 downto 0);
	signal pc : std_logic_vector(14 downto 0);
	signal pc_branch : std_logic;
	signal SREG : std_logic_vector(7 downto 0);
	signal mult : std_logic_vector(15 downto 0);
	signal src_reg  : std_logic_vector(7 downto 0);
	signal dest_reg : std_logic_vector(15 downto 0);
	signal insts : std_logic_vector(15 downto 0);
	signal exec_counter : exec_c;
	signal pipeline_counter : integer range 0 to 3;
	component multiply is
		PORT
		(
			dataa		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
			datab		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
			result		: OUT STD_LOGIC_VECTOR (15 DOWNTO 0)
		);
	end component;

begin

	mult : multiply
			port map(
				dataa => dataa,
				datab => datab,
				result => mult_out
			);
			
	inst_mem_addr_0 <= pc & '0';
	inst_mem_addr_1 <= pc & '1';
	insts <= inst_mem_out_1 & inst_mem_out_0;
	
	process (reset_n,clock) begin
		if rising_edge(clock) then
			if(reset_n = '0') then
				pc <= X"0000";
			else
				case insts
					when "000011----------"	=> --add
						
						dest_reg(7 downto 0) <= regs(to_integer(unsigned(insts(8 downto 4))));
						src_reg <= regs(to_integer(unsigned(insts(9) & insts(3 downto 0))));
						
						regs(to_integer(unsigned(insts(8 downto 4)))) <= std_logic_vector(unsigned(dest_reg(7 downto 0)) + to_integer(unsigned(src_reg)));
					when "10010110--------" => --adiw
						dest_reg(7 downto 0) <= regs(to_integer(unsigned(24+unsigned(insts(5 downto 4) & '0'))));
						dest_reg(15 downto 8) <= regs(to_integer(unsigned(24+unsigned(insts(5 downto 4) & '1'))));
						immidiate <= insts(7 downto 6) & insts(3 downto 0);
						
						reg_16buf <= std_logic_vector(unsigned(dest_reg) + to_integer(unsigned(immidiate)));
						
						regs(to_integer(unsigned(24+unsigned(insts(5 downto 4) & '1')))) <= reg_16buf(15 downto 8);
						regs(to_integer(unsigned(24+unsigned(insts(5 downto 4) & '0')))) <= reg_16buf(7 downto 0);	
					when "000110----------" =>--sub
						dest_reg(7 downto 0) <= regs(to_integer(unsigned(dest)));
						src_reg <= regs(to_integer(unsigned(insts(9) & insts(3 downto 0))));
						regs(to_integer(unsigned(dest))) <= std_logic_vector(unsigned(dest_reg(7 downto 0)) - to_integer(unsigned(src_reg)));
					when "1001010-----0000" => --com
						dest_reg(7 downto 0) <= regs(to_integer(unsigned(8 downto 4)));
						regs(to_integer(unsigned(dest))) <= std_logic_vector(X"FF" - unsigned(dest_reg(7 downto 0)));
					when "1110------------" => --ldi
						immidiate <= insts(11 downto 8) & insts(3 downto 0);
						regs(to_integer(unsigned(insts(7 downto 4) + 16))) <= immidiate;
					when "1100------------" => --rjmp
						pc <= std_logic_vector((signed(pc) + to_integer(signed(insts(11 downto 0)))) + 1);
					when "0000000000000000" =>  --nop
						null;
				end case;
			end if;
		end if;
	end process;

end logic;

