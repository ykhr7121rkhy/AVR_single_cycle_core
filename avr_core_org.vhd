
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
	--type inst_array is array(0 to 3) of instructs;
	type reg_8bit_array is array (0 to 4) of std_logic_vector(7 downto 0);
	type reg_16bit_array is array (0 to 4) of std_logic_vector(15 downto 0);
	type c_array is array (0 to 4) of integer range 0 to 15;
	type reg_st is (fetch,fetch1,execute);
	type reg_st_array is array (0 to 4) of reg_st;
	type reg_no is array (0 to 4) of integer range 0 to 31;
	signal regs : regarray;
	signal immidiate : reg_8bit_array;
	signal reg_16buf : reg_16bit_array;
	signal pc : integer range -32768	to 32767;
	signal pc_branch : integer range -32768 to 32767;
	signal SREG : std_logic_vector(7 downto 0);
	signal mult_out : std_logic_vector(15 downto 0);
	signal multa,multb : std_logic_vector(7 downto 0);
	signal src_reg  : reg_8bit_array;
	signal dest_reg : reg_16bit_array;
	signal insts : reg_16bit_array;
	signal reg_inst : instructs;
	signal counter : c_array;
	signal tmp0,tmp1,tmp2,tmp3,tmp4 : reg_16bit_array;
	signal inst_mem_addr_0,inst_mem_addr_1 : std_logic_vector(8 downto 0);
	signal inst_mem_out_0,inst_mem_out_1 : std_logic_vector(7 downto 0);
	signal rst_flg : std_logic_vector(4 downto 0);
	signal state : reg_st_array;
	signal rst_counter : integer range 0 to 15;
	signal dest_reg_no : reg_no;
	signal src_reg_no : reg_no;
	signal forwarding_src_out : std_logic_vector(7 downto 0);
	signal forwarding_dst_out : std_logic_vector(7 downto 0);
	signal forwarding_src_flag : std_logic;
	signal forwarding_dst_flag : std_logic;
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

pipeline_forwarding:	
	for j in 0 to 4 generate
		for i in 0 to 4 generate
			if(counter(i) = 1 and counter(j) = 2) then
				if(dest_reg_no(j) = dest_reg_no(i)) then
					forwarding_dst_flag <= '1';
					forwarding_dst_out <= regs(dest_reg_no(j));
				end if;
				if(dest_reg_no(j) = src_reg_no(i)) then
					forwarding_src_flag <= '1';
					forwarding_src_out <= regs(src_reg_no(j));
				end if;
			end if;
		end generate;
	end generate;
	
main_process:
	process (reset_n,clock) begin
		if rising_edge(clock) then
			if(reset_n = '0') then
				pc <= 0;
				for i in 0 to 4 loop
					insts(i) <= (others => '0');
				end loop;
				for i in 0 to 31 loop
					regs(i) <= (others => '0');
				end loop;
				state(0) <= fetch;
				state(1) <= fetch1;
				state(2) <= execute;
				state(3) <= execute;
				state(4) <= execute;
				counter(0) <= 0;
				counter(1) <= 0;
				counter(2) <= 0;
				counter(3) <= 1;
				counter(4) <= 2;
				rst_counter <= 0;
			else
				if(pc >= 32767) then
					pc <= 0;
				end if;
				if(rst_counter <= 5) then
					rst_counter <= rst_counter + 1;
				end if;
				for i in 0 to 4 loop
					case state(i) is
					when fetch =>
						inst_mem_addr_0 <= std_logic_vector(to_unsigned(pc,8)) & '0';
						inst_mem_addr_1 <= std_logic_vector(to_unsigned(pc,8)) & '1';
						state(i) <= fetch1;
					when fetch1 =>
						insts(i) <= inst_mem_out_1 & inst_mem_out_0;
						state(i) <= execute;
					when execute =>
						if(insts(i)(15 downto 10) = "000011")	then--add
							case counter(i) is
							when 0 =>
								dest_reg_no(i) <= to_integer(unsigned(insts(i)(8 downto 4)));
								src_reg_no(i) <= to_integer(unsigned(insts(i)(9) & insts(i)(3 downto 0)))
								counter(i) <= counter(i) + 1;
							when 1 => 	
								counter(i) <= counter(i) + 1;
							when 2 =>
								if(forwarding_dst_flag = '1' and forwarding_src_flag = '1') then
									regs(dest_reg_no(i)) <= std_logic_vector(unsigned(forwarding_dst_out) + to_integer(unsigned(forwarding_src_out)));
								elsif (forwarding_dst_flag = '1' and forwarding_src_flag = '0') then
									regs(dest_reg_no(i)) <= std_logic_vector(unsigned(forwarding_dst_out) + to_integer(unsigned(regs(src_reg_no(i)))));
								elsif (forwarding_dst_flag = '0' and forwarding_src_flag = '1') then
									regs(dest_reg_no(i)) <= std_logic_vector(unsigned(regs(dest_reg_no(i))) + to_integer(unsigned(forwarding_src_out)));
								else
									regs(dest_reg_no(i)) <= std_logic_vector(unsigned(regs(dest_reg_no(i))) + to_integer(unsigned(regs(src_reg_no(i)))));
								end if;
								
								pc <= pc + 1;
								counter(i) <= 0;
								reg_inst <= add;
							when others =>
								counter(i) <= 0;
							end case;
						elsif (insts(i)(15 downto 8) = "10010110") then--adiw
							case counter(i) is
							when 0 =>
								dest_reg(i)(7 downto 0) <= regs(to_integer(unsigned(24+unsigned(insts(i)(5 downto 4) & '0'))));
								dest_reg(i)(15 downto 8) <= regs(to_integer(unsigned(24+unsigned(insts(i)(5 downto 4) & '1'))));
								immidiate(i) <= "00" & insts(i)(7 downto 6) & insts(i)(3 downto 0);
								counter(i) <= counter(i) + 1;
							when 1 =>
								reg_16buf(i)<= std_logic_vector(unsigned(dest_reg(i)) + to_integer(unsigned(immidiate(i))));
								counter(i) <= counter(i) + 1;
							when 2 =>
								regs(to_integer(unsigned(24+unsigned(insts(i)(5 downto 4) & '1')))) <= reg_16buf(i)(15 downto 8);
								regs(to_integer(unsigned(24+unsigned(insts(i)(5 downto 4) & '0')))) <= reg_16buf(i)(7 downto 0);
								pc <= pc + 1;
								counter(i) <= 0;
								reg_inst <= adiw;
							when others =>
								counter(i) <= 0;
							end case;		
						elsif (insts(i)(15 downto 10) = "000110") then --sub
							case counter(i) is
							when 0 =>
								dest_reg_no(i) <= to_integer(unsigned(insts(i)(8 downto 4)));
								src_reg_no(i) <= to_integer(unsigned(insts(i)(9) & insts(i)(3 downto 0)))
								counter(i) <= counter(i) + 1;
							when 1 => 	
								counter(i) <= counter(i) + 1;
							when 2 =>
								if(forwarding_dst_flag = '1' and forwarding_src_flag = '1') then
									regs(dest_reg_no(i)) <= std_logic_vector(unsigned(forwarding_dst_out) - to_integer(unsigned(forwarding_src_out)));
								elsif (forwarding_dst_flag = '1' and forwarding_src_flag = '0') then
									regs(dest_reg_no(i)) <= std_logic_vector(unsigned(forwarding_dst_out) - to_integer(unsigned(regs(src_reg_no(i)))));
								elsif (forwarding_dst_flag = '0' and forwarding_src_flag = '1') then
									regs(dest_reg_no(i)) <= std_logic_vector(unsigned(regs(dest_reg_no(i))) - to_integer(unsigned(forwarding_src_out)));
								else
									regs(dest_reg_no(i)) <= std_logic_vector(unsigned(regs(dest_reg_no(i))) - to_integer(unsigned(regs(src_reg_no(i)))));
								end if;
								
								pc <= pc + 1;
								counter(i) <= 0;
								reg_inst <= sub;
							
						elsif (insts(i)(15 downto 9) = "1001010" and insts(i)(3 downto 0) = "0000") then	--com
							case counter(i) is
							when 0 =>
								dest_reg_no(i) <= to_integer(unsigned(insts(i)(8 downto 4)));
								counter(i) <= counter(i) + 1;
							when 1 => 	
								counter(i) <= counter(i) + 1;
							when 2 =>
								if (forwarding_dst_flag = '0' and forwarding_src_flag = '1') then
									regs(dest_reg_no(i)) <= std_logic_vector(255-unsigned(regs(forwarding_dst_out)));
								else
									regs(dest_reg_no(i)) <= std_logic_vector(255-unsigned(regs(dest_reg_no(i))));
								end if;
								
								pc <= pc + 1;
								counter(i) <= 0;
								reg_inst <= sub;			 
						elsif(insts(i)(15 downto 10) = "100111" ) then --mul
							case counter(i) is
							when 0 =>
								dest_reg_no(i) <= 
								src_reg_no(i) <= to_integer(unsigned(insts(i)(9) & insts(i)(3 downto 0)))
								counter(i) <= counter(i) + 1;
							when 1 =>
								multa <= regs(to_integer(unsigned(insts(i)(8 downto 4))));
								multb <= regs(to_integer(unsigned(insts(i)(9) & insts(i)(3 downto 0))));
								counter(i) <= counter(i) + 1;
							when 2 =>
								regs(1) <= mult_out(15 downto 8);
								regs(0) <= mult_out(7 downto 0);
								pc <= pc + 1;
								counter(i) <= 0;
								if(forwarding_dst_flag = '1' and forwarding_src_flag = '1') then
									regs(dest_reg_no(i)) <= std_logic_vector(unsigned(forwarding_dst_out) - to_integer(unsigned(forwarding_src_out)));
								elsif (forwarding_dst_flag = '1' and forwarding_src_flag = '0') then
									regs(dest_reg_no(i)) <= std_logic_vector(unsigned(forwarding_dst_out) - to_integer(unsigned(regs(src_reg_no(i)))));
								elsif (forwarding_dst_flag = '0' and forwarding_src_flag = '1') then
									regs(dest_reg_no(i)) <= std_logic_vector(unsigned(regs(dest_reg_no(i))) - to_integer(unsigned(forwarding_src_out)));
								else
									regs(dest_reg_no(i)) <= std_logic_vector(unsigned(regs(dest_reg_no(i))) - to_integer(unsigned(regs(src_reg_no(i)))));
								end if;
								reg_inst <= mul;
							when others =>
								counter(i) <= 0;
							end case;
						elsif(insts(i)(15 downto 12) = "1110") then --ldi
							case counter(i) is
							when 0 =>
								counter(i) <= counter(i) + 1;
							when 1 =>
								counter(i) <= counter(i) + 1;
							when 2 =>
								regs(to_integer(unsigned(('0' & insts(i)(7 downto 4))) + 16)) <= insts(i)(11 downto 8) & insts(i)(3 downto 0);
								pc <= pc + 1;
								counter(i) <= 0;
								reg_inst <= ldi;
							when others =>
								counter(i) <= 0;
							end case;
						elsif(insts(i)(15 downto 12) = "1100") then --rjmp
							case counter(i) is
							when 0 =>
								counter(i) <= counter(i) + 1;
							when 1 => 
								counter(i) <= counter(i) + 1;
								tmp0(i) <= insts(i)(11) & insts(i)(11)& insts(i)(11) & insts(i)(11) & insts(i)(11 downto 0);
							when 2 =>
								pc <= to_integer(1 + (pc + signed(tmp0(i))));
								reg_inst <= rjmp;
								counter(i) <= 0;
							when others =>
								counter(i) <= 0;
							end case;
								
								
						elsif(insts(i) = "0000000000000000") then  --nop
							case counter(i) is
							when 0 =>
								counter(i) <= counter(i) + 1;
							when 1 => 
								counter(i) <= counter(i) + 1;
							when 2 =>
								if(rst_counter >= 9) then
									pc <= pc + 1;
								end if;
								reg_inst <= nop;
								counter(i) <= 0;
							when others =>
								counter(i) <= 0;
							end case;
						else --nop
							case counter(i) is
							when 0 =>
								counter(i) <= counter(i) + 1;
							when 1 => 
								counter(i) <= counter(i) + 1;
							when 2 =>
							if(rst_counter >= 5) then
									pc <= pc + 1;
							end if;
								reg_inst <= nop;
								counter(i) <= 0;
							when others =>
								counter(i) <= 0;
							end case;
						end if;
						if counter(i) = 2 then
							state(i) <= fetch;
						end if;
					when others =>
						null;
					end case;
				end loop;
			end if;
		end if;
	end process;

end logic;

