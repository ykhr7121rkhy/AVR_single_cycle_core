
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity avr_core is
	port(
		reset_n : in std_logic;
		clock : in std_logic
		--out_addr_bus : out std_logic_vector(15 downto 0);
		--inout_data_bus : inout std_logic_vector(7 downto 0);
	);
end avr_core;

architecture logic of avr_core is
	type regarray is array (0 to 31) of std_logic_vector(7 downto 0); -- general purpose regs
	type instructs is (add,adc,adiw,sub,subi,sbc,sbci,sbiw,and0,andi,or0,ori,eor,com,neg,sbr,cbr,inc,dec,tst,clr,ser,mul,
									rjmp,ijmp,rcall,call,ret,reti,cpse,cp,cpc,cpi,sbrc,sbrs,sbic,sbis,brbs,brbc,breq,brne,brcs,brcc,brsh,brlo,brmi,brpl,brge,brlt,brhs,brhc,brts,brtc,brvs,brvc,brie,brid,
									mov,ldi,lds,ld,ldd,sts,st,std0,lpm,in0,out0,push,pop,
									lsl,lsr,rol0,ror0,asr,swap,bset,bclr,sbi,cbi,bst,bld,sec,clc,sen,cln,sez,clz,sei,cli,ses,cls,sev,clv,set,clt,seh,clh,nop,sleep,wdr);
	type inst_array is array(0 to 3) of instructs;
	type reg_8bit_array is array (0 to 3) of std_logic_vector(7 downto 0);
	type reg_16bit_array is array (0 to 3) of std_logic_vector(15 downto 0);
	type c_array is array (0 to 3) of integer range 0 to 15;
	signal regs : regarray;
	signal immidiate : reg_8bit_array;
	signal reg_16buf : reg_16bit_array;
	signal pc : integer range -32768	to 32767;
	signal pc_branch : std_logic;
	signal SREG : std_logic_vector(7 downto 0);
	signal mult_out : std_logic_vector(15 downto 0);
	signal multa,multb : std_logic_vector(7 downto 0);
	signal src_reg  : reg_8bit_array;
	signal dest_reg : reg_16bit_array;
	signal insts : reg_16bit_array;
	signal reg_inst : inst_array;
	signal pipeline_counter : integer range 0 to 3;
	signal counter : c_array;
	signal tmp0,tmp1,tmp2,tmp3,tmp4 : reg_16bit_array;
	signal inst_mem_addr_0,inst_mem_addr_1 : std_logic_vector(8 downto 0);
	signal inst_mem_out_0,inst_mem_out_1 : std_logic_vector(7 downto 0);
	component multiply is
		PORT
		(
			clock		: IN STD_LOGIC ;
			dataa		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
			datab		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
			result		: OUT STD_LOGIC_VECTOR (15 DOWNTO 0)
		);
	end component;
	component inst_mem IS
		PORT
		(
			address_a		: IN STD_LOGIC_VECTOR (8 DOWNTO 0);
			address_b		: IN STD_LOGIC_VECTOR (8 DOWNTO 0);
			clock		: IN STD_LOGIC  := '1';
			q_a		: OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
			q_b		: OUT STD_LOGIC_VECTOR (7 DOWNTO 0)
		);
	END component;

begin

	mult : multiply
		port map(
			clock => clock,
			dataa => multa,
			datab => multb,
			result => mult_out
		);

	mem : inst_mem
		port map(
			address_a => inst_mem_addr_0,
			address_b => inst_mem_addr_1,
			clock => clock,
			q_a => inst_mem_out_0,
			q_b => inst_mem_out_1
		);
			
	inst_mem_addr_0 <= std_logic_vector(to_unsigned(pc,8)) & '0';
	inst_mem_addr_1 <= std_logic_vector(to_unsigned(pc,8)) & '1';
	
	
	process (reset_n,clock) begin
		if rising_edge(clock) then
			if(reset_n = '0') then
				pc <= 0;
				for i in 0 to 3 loop
					insts(i) <= (others => '0');
				end loop;
				insts(0) <= inst_mem_out_1 & inst_mem_out_0;
				counter(0) <= 0;
				counter(1) <= 3;
				counter(2) <= 2;
				counter(3) <= 1;
			else
				if(pc >= 32767) then
					pc <= 0;
				end if;

				for i in 0 to 3 loop
					if(insts(i)(15 downto 10) = "000011")	then--add
						case counter(i) is
						when 0 =>
							reg_inst(i) <= add;
							dest_reg(i)(7 downto 0) <= regs(to_integer(unsigned(insts(i)(8 downto 4))));
							src_reg(i) <= regs(to_integer(unsigned(insts(i)(9) & insts(i)(3 downto 0))));
							counter(i) <= counter(i) + 1;
						when 1 => 
							counter(i) <= counter(i) + 1;
						when 2 => 
							counter(i) <= counter(i) + 1;
						when 3 =>
							regs(to_integer(unsigned(insts(i)(8 downto 4)))) <= std_logic_vector(unsigned(dest_reg(i)(7 downto 0)) + to_integer(unsigned(src_reg(i))));
							pc <= pc + 1;
							counter(i) <= 0;
						when others =>
							counter(i) <= 0;
						end case;
					elsif (insts(i)(15 downto 8) = "10010110") then--adiw
						case counter(i) is
						when 0 =>
							reg_inst(i) <= adiw;
							dest_reg(i)(7 downto 0) <= regs(to_integer(unsigned(24+unsigned(insts(i)(5 downto 4) & '0'))));
							dest_reg(i)(15 downto 8) <= regs(to_integer(unsigned(24+unsigned(insts(i)(5 downto 4) & '1'))));
							immidiate(i) <= "00" & insts(i)(7 downto 6) & insts(i)(3 downto 0);
							counter(i) <= counter(i) + 1;
						when 1 =>
							reg_16buf(i)<= std_logic_vector(unsigned(dest_reg(i)) + to_integer(unsigned(immidiate(i))));
							counter(i) <= counter(i) + 1;
						when 2 => 
							counter(i) <= counter(i) + 1;
						when 3 =>
							regs(to_integer(unsigned(24+unsigned(insts(i)(5 downto 4) & '1')))) <= reg_16buf(i)(15 downto 8);
							regs(to_integer(unsigned(24+unsigned(insts(i)(5 downto 4) & '0')))) <= reg_16buf(i)(7 downto 0);
							pc <= pc + 1;
							counter(i) <= 0;
						when others =>
							counter(i) <= 0;
						end case;		
					elsif (insts(i)(15 downto 10) = "000110") then --sub
						case counter(i) is
						when 0 =>
							reg_inst(i) <= sub;
							dest_reg(i)(7 downto 0) <= regs(to_integer(unsigned(insts(i)(8 downto 4))));
							src_reg(i) <= regs(to_integer(unsigned(insts(i)(9) & insts(i)(3 downto 0))));
							counter(i) <= counter(i) + 1;
						when 1 =>
							counter(i) <= counter(i) + 1;
						when 2 => 
							counter(i) <= counter(i) + 1;
						when 3 =>
							regs(to_integer(unsigned(insts(i)(8 downto 4)))) <= std_logic_vector(unsigned(dest_reg(i)(7 downto 0)) - to_integer(unsigned(src_reg(i))));
							pc <= pc + 1;
							counter(i) <= 0;
						when others =>
							counter(i) <= 0;
						end case; 
					elsif (insts(i)(15 downto 9) = "1001010" and insts(i)(3 downto 0) = "0000") then	--com
						case counter(i) is
						when 0 =>
							reg_inst(i) <= com;						
							counter(i) <= counter(i) + 1;
						when 1 =>
							counter(i) <= counter(i) + 1;
						when 2 => 
							counter(i) <= counter(i) + 1;
						when 3 =>
							regs(to_integer(unsigned(insts(i)(8 downto 4)))) <= std_logic_vector(X"FF" - unsigned(regs(to_integer(unsigned(insts(i)(8 downto 4))))));
							pc <= pc + 1;
							counter(i) <= 0;
						when others =>
							counter(i) <= 0;
						end case; 					 
							
					elsif(insts(i)(15 downto 10) = "100111" ) then --mul
						case counter(i) is
						when 0 =>
							reg_inst(i) <= mul;
							multa <= regs(to_integer(unsigned(insts(i)(8 downto 4))));
							multb <= regs(to_integer(unsigned(insts(i)(9) & insts(i)(3 downto 0))));
							counter(i) <= counter(i) + 1;
						when 1 =>
							counter(i) <= counter(i) + 1;
						when 2 => 
							counter(i) <= counter(i) + 1;
						when 3 =>
							regs(1) <= mult_out(15 downto 8);
							regs(0) <= mult_out(7 downto 0);
							pc <= pc + 1;
							counter(i) <= 0;
						when others =>
							counter(i) <= 0;
						end case;
					elsif(insts(i)(15 downto 12) = "1110") then --ldi
						case counter(i) is
						when 0 =>
							reg_inst(i) <= ldi;
							counter(i) <= counter(i) + 1;
						when 1 =>
							counter(i) <= counter(i) + 1;
						when 2 => 
							counter(i) <= counter(i) + 1;
						when 3 =>
							regs(to_integer(unsigned(insts(i)(7 downto 4)) + 16)) <= insts(i)(11 downto 8) & insts(i)(3 downto 0);
							pc <= pc + 1;
							counter(i) <= 0;
						when others =>
							counter(i) <= 0;
						end case;
					elsif(insts(i)(15 downto 12) = "1100") then --rjmp
						case counter(i) is
						when 0 =>
							reg_inst(i) <= rjmp;
							counter(i) <= counter(i) + 1;
						when 1 =>
							counter(i) <= counter(i) + 1;
						when 2 => 
							counter(i) <= counter(i) + 1;
							tmp0(i) <= insts(i)(11) & insts(i)(11)& insts(i)(11) & insts(i)(11) & insts(i)(11 downto 0);
						when 3 =>
							pc <= to_integer(pc + signed(tmp0(i)));
							counter(i) <= 0;
						when others =>
							counter(i) <= 0;
						end case;
							
							
					elsif(insts(i) = "0000000000000000") then  --nop
						case counter(i) is
						when 0 =>
							reg_inst(i) <= nop;
							counter(i) <= counter(i) + 1;
						when 1 =>
							counter(i) <= counter(i) + 1;
						when 2 => 
							counter(i) <= counter(i) + 1;
						when 3 =>
							pc <= pc + 1;
							counter(i) <= 0;
						when others =>
							counter(i) <= 0;
						end case;
					else
							null;
					end if;
					if counter(i) = 3 then
						insts(i) <= inst_mem_out_1 & inst_mem_out_0;
					--	for j in 0 to 2 loop
						--	insts(j+1) <= insts(j);
					--	end loop;
					end if;
				end loop;
			end if;
		end if;
	end process;

end logic;

