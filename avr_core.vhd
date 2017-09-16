
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity avr_core is
	port(
		reset_n : in std_logic;
		clock : in std_logic;
		instructs_code : in std_logic_vector(15 downto 0);
		dest : in std_logic_vector(7 downto 0);
		src : in std_logic_vector(7 downto 0);
	);
end avr_core;

architecture logic of avr_core is
	type regarray is array (0 to 31) of (std_logic_vector(7 downto 0));
	type instructs is array (add,adc,adiw,sub,subi,sbc,sbci,sbiw,_and,andi,_or,ori,eor,com,neg,sbr,cbr,inc,dec,tst,clr,ser,mul,
										rjmp,ijmp,rcall,call,ret,reti,cpse,cp,cpc,cpi,sbrc,sbrs,sbic,sbis,brbs,brbc,breq,brne,brcs,brcc,brsh,brlo,brmi,brpl,brge,brlt,brhs,brhc,brts,brtc,brvs,brvc,brie,brid,
										mov,ldi,lds,ld,ldd,sts,st,_std,lpm,_in,_out,push,pop,
										lsl,lsr,_rol,_ror,asr,swap,bset,bclr,sbi,cbi,bst,bld,sec,clc,sen,cln,sez,clz,sei,cli,ses,cls,sev,clv,set,clt,seh,clh,nop,sleep,wdr);
	signal regs : regarray;
	signal insts : instructs;
	signal immidiate : std_logic_vector(7 downto 0);
	signal reg_16buf : std_logic_vector(7 downto 0);
	signal pc : std_logic_vector(15 downto 0);
	signal pc_branch : std_logic;

	signal mult : std_logic_vector(15 downto 0);
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
			
	insts <= add when (instructs_code = X"0001") else
				adiw when (instructs_code = X"0002") else
				sub when (instructs_code = X"0003") else
				com when (instructs_code = X"0004") else
				nop;
	process (reset_n,clock) begin
		if rising_edge(clock) then
			if(reset_n = '0') then
				pc <= X"0000";
			else
				if(pc_branch = '0') then
					pc <= std_logic_vector((unsigned(pc) + 1));
				end if;
				case insts
					when add =>
						dest_reg(7 downto 0) <= regs(to_integer(unsigned(dest)));
						src_reg <= regs(to_integer(unsigned(src)));
						
						regs(to_integer(unsigned(dest))) <= std_logic_vector(unsigned(dest_reg(7 downto 0)) + to_integer(unsigned(src_reg)));
					when adiw =>
						dest_reg(7 downto 0) <= regs(to_integer(unsigned(dest)));
						dest_reg(15 downto 8) <= regs(to_integer(unsigned(dest) + 1));
						immidiate <= src;
						
						reg_16buf <= std_logic_vector(unsigned(dest_reg) + to_integer(unsigned(immidiate)));
						
						regs(to_integer(unsigned(dest) + 1)) <= reg_16buf(15 downto 8);
						regs(to_integer(unsigned(dest))) <= reg_16buf(7 downto 0);	
					when sub =>
						dest_reg(7 downto 0) <= regs(to_integer(unsigned(dest)));
						src_reg <= regs(to_integer(unsigned(src)));
						regs(to_integer(unsigned(dest))) <= std_logic_vector(unsigned(dest_reg(7 downto 0)) - to_integer(unsigned(src_reg)));
					when com =>
						dest_reg(7 downto 0) <= regs(to_integer(unsigned(dest)));
						src_reg <= regs(to_integer(unsigned(src)));

						regs(to_integer(unsigned(dest))) <= std_logic_vector(X"FF" - unsigned(dest_reg(7 downto 0)));
					when ldi =>
						immidiate <= src;
						
						regs(to_integer(unsigned(dest))) <= immidiate;
					when nop =>
						null;
					end case;
				end case;
			end if;
		end if;
	end process;

end logic;

