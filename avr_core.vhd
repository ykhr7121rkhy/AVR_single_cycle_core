
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--ramsize : 64kB

entity avr_core is
	port(
		reset_n : in std_logic;
		clock : in std_logic;
		portA : inout std_logic_vector(7 downto 0);
		portB : inout std_logic_vector(7 downto 0);
		portC : inout std_logic_vector(7 downto 0);
		portD : inout std_logic_vector(7 downto 0)
		--out_addr_bus : out std_logic_vector(15 downto 0);
		--inout_data_bus : inout std_logic_vector(7 downto 0);
	);
end avr_core;

architecture logic of avr_core is
	type regarray is array (0 to 31) of std_logic_vector(7 downto 0); -- general purpose regs
	type sp_regarray is array (0 to 63) of std_logic_vector(7 downto 0); -- spetial function regs
	type instructs is (add,adc,adiw,sub,subi,sbc,sbci,sbiw,and0,andi,or0,ori,eor,com,neg,sbr,cbr,inc,dec,tst,clr,ser,mul,
									rjmp,ijmp,jmp,rcall,icall,call,ret,reti,cpse,cp,cpc,cpi,sbrc,sbrs,sbic,sbis,brbs,brbc,breq,brne,brcs,brcc,brsh,brlo,brmi,brpl,brge,brlt,brhs,brhc,brts,brtc,brvs,brvc,brie,brid,
									mov,ldi,lds,ld,ldd,sts,st,std0,lpm,in0,out0,push,pop,lac,las,lat,ldX,ldXp,ldmX,
									lsl,lsr,rol0,ror0,asr,swap,bset,bclr,sbi,cbi,bst,bld,sec,clc,sen,cln,sez,clz,sei,cli,ses,cls,sev,clv,set,clt,seh,clh,nop,sleep,wdr);
	type c_array is array (0 to 4) of integer range 0 to 15;
	type reg_st is (fetch,decode,execute);
	signal genpurp_regs : regarray;
	signal spetial_func_regs : sp_regarray;
	signal pc : integer range -65536 to 65535;
	signal pc_in : integer range -32768 to 32767;
	signal sreg : std_logic_vector(7 downto 0);
	signal I_flag,T_flag,H_flag,S_flag,V_flag,N_flag,Z_flag,C_flag : std_logic;
	signal mult_out : std_logic_vector(15 downto 0);
	signal multa,multb : std_logic_vector(7 downto 0);
	signal insts : std_logic_vector(15 downto 0);
	signal reg_inst : instructs;
	signal inst_mem_addr_msb,inst_mem_addr_lsb : std_logic_vector(15 downto 0);
	signal inst_mem_out_msb,inst_mem_out_lsb : std_logic_vector(15 downto 0);
	signal rst_flg : std_logic_vector(4 downto 0);
	signal state : reg_st;
	signal rst_counter : integer range 0 to 15;
	signal forwarding_src_out : std_logic_vector(7 downto 0);
	signal forwarding_dst_out : std_logic_vector(7 downto 0);
	signal forwarding_src_flag : std_logic;
	signal forwarding_dst_flag : std_logic;
	signal stack_pointer : integer range 0 to 65535;
	signal ram0_addr : std_logic_vector(15 downto 0);
	signal ram0_in : std_logic_vector(15 downto 0);
	signal ram0_wren : std_logic;
	signal ram0_out : std_logic_vector(15 downto 0);
	signal inst_mem_data_msb : std_logic_vector(7 downto 0);
	signal inst_mem_data_lsb : std_logic_vector(7 downto 0); 
	signal inst_mem_write : std_logic;
	signal inst_mem_burstcount : std_logic_vector(1 downto 0);
	signal inst_mem_csr_addr : std_logic;
	signal inst_mem_csr_read : std_logic;
	signal inst_mem_csr_writedata : std_logic_vector(31 downto 0);
	signal inst_mem_csr_write : std_logic; 
	signal flash_wait_count : integer range 0 to 7;
	signal call_jmp_32bit_flag : std_logic;
	signal call_jmp_pc_tmp : std_logic_vector(21 downto 0);
	signal ram_addr_msb : std_logic_vector(15 downto 0);
	signal ram_addr_lsb : std_logic_vector(15 downto 0);
   signal ram_in_msb : std_logic_vector(7 downto 0);
	signal ram_in_lsb : std_logic_vector(7 downto 0);
	signal ram_wr_msb : std_logic;
	signal ram_wr_lsb : std_logic;
	signal ram_out_msb : std_logic_vector(7 downto 0);
	signal ram_out_lsb : std_logic_vector(7 downto 0);
	signal pc_to_inst_wait : integer range 0 to 3;
	
	component multiply is
		PORT
		(
			clock		: IN STD_LOGIC ;
			dataa		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
			datab		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
			result		: OUT STD_LOGIC_VECTOR (15 DOWNTO 0)
		);
	end component;
--	component inst_memory IS
--		port (
--			clock                   : in  std_logic;                   
--			reset_n                 : in  std_logic;                    
--			avmm_data_addr          : in  std_logic_vector(14 downto 0); 
--			avmm_data_read          : in  std_logic;                     
--			avmm_data_writedata     : in  std_logic_vector(31 downto 0); 
--			avmm_data_write         : in  std_logic;                     
--			avmm_data_readdata      : out std_logic_vector(31 downto 0);  
--			avmm_data_waitrequest   : out std_logic;                                 
--			avmm_data_readdatavalid : out std_logic;                  
--			avmm_data_burstcount    : in  std_logic_vector(1 downto 0); 
--			avmm_csr_addr           : in  std_logic;                 
--			avmm_csr_read           : in  std_logic;
--			avmm_csr_writedata      : in  std_logic_vector(31 downto 0);
--			avmm_csr_write          : in  std_logic;
--			avmm_csr_readdata       : out std_logic_vector(31 downto 0)
--		);
--	END component;
	
	component ram0 IS
		PORT
		(
			address_a		: IN STD_LOGIC_VECTOR (15 DOWNTO 0);
			address_b		: IN STD_LOGIC_VECTOR (15 DOWNTO 0);
			clock		: IN STD_LOGIC;
			data_a		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
			data_b		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
			wren_a		: IN STD_LOGIC ;
			wren_b		: IN STD_LOGIC ;
			q_a		: OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
			q_b		: OUT STD_LOGIC_VECTOR (7 DOWNTO 0)
		);
	END component;
	
		
	component inst_mem IS
		PORT
		(
			address_a		: IN STD_LOGIC_VECTOR (15 DOWNTO 0);
			address_b		: IN STD_LOGIC_VECTOR (15 DOWNTO 0);
			clock		: IN STD_LOGIC ;
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

--	mem : inst_memory
--		port map (
--			clock => clock,
--			reset_n => reset_n,
--			avmm_data_addr => inst_mem_addr_0(14 downto 0)
--			avmm_data_read => inst_mem_read,
--			avmm_data_writedata => inst_mem_writedata,
--			avmm_data_write => inst_mem_write,
--			avmm_data_readdata => inst_mem_out_0,
--			avmm_data_waitrequest => open,
--			avmm_data_readdatavalid => open,
--			avmm_data_burstcount => inst_mem_burstcount,
--			avmm_csr_addr => inst_mem_csr_addr,
--			avmm_csr_read => inst_mem_csr_read,
--			avmm_csr_writedata => inst_mem_csr_writedata,
--			avmm_csr_write => inst_mem_csr_write,
--			avmm_csr_readdata => open
--		);
		
	--inst_mem_writedata <= (others => '0');
	--inst_mem_write <= '0';
	--inst_mem_burstcount <= "01";
	--inst_mem_csr_addr <= 'Z';
	--inst_mem_csr_writedata <= (others => 'Z');
	--inst_mem_csr_write <= 'Z';

	inst_mem_0 : inst_mem
		PORT map
		(
			address_a => inst_mem_addr_msb,
			address_b => inst_mem_addr_lsb,
			clock	=> clock,
			q_a => inst_mem_data_msb,
			q_b => inst_mem_data_lsb
		);


	
	
	ram_0_0 : ram0
		PORT map
		(
			address_a => ram_addr_msb,
			address_b => ram_addr_lsb,
			clock	=> clock,
			data_a => ram_in_msb,
			data_b => ram_in_lsb,
			wren_a => ram_wr_msb,
			wren_b => ram_wr_lsb,
			q_a	 => ram_out_msb,
			q_b	 => ram_out_lsb
		);
		
		
	inst_mem_addr_msb <= std_logic_vector(to_unsigned(pc,15)) & '1';
	inst_mem_addr_lsb <= std_logic_vector(to_unsigned(pc,15)) & '0';
	
	
main_process:
	process (reset_n,clock)
		variable dest_reg_no,src_reg_no : integer range 0 to 31;
		variable io_reg_no : integer range 0 to 63;
		variable bit_no : integer range 0 to 7;
		variable immidiate : std_logic_vector(15 downto 0);
		variable reg_16buf : std_logic_vector(15 downto 0);
		variable dest_reg,src_reg : std_logic_vector(15 downto 0); 
		variable N,V,C : std_logic;
		variable result : std_logic_vector(15 downto 0);
		variable rr3,rd3,r3,rr7,rd7,r7 : std_logic;
		variable pc_tmp : std_logic_vector(15 downto 0);
		variable ram_data_tmp : std_logic_vector(15 downto 0);
		variable tmp : std_logic_vector(7 downto 0);
		variable tmp16 : std_logic_vector(15 downto 0);
		variable X_reg,Y_reg,Z_reg : std_logic_vector(7 downto 0);
		variable exec_counter : integer range 0 to 16;
	begin
		
		if rising_edge(clock) then
			if(reset_n = '0') then
				pc <= 0;
				stack_pointer <= 65535;
				for i in 0 to 31 loop
					genpurp_regs(i) <= (others => '0');
				end loop;
				state <= fetch;
				call_jmp_32bit_flag <= '0';
			else
				for i in 0 to 7 loop
					if spetial_func_regs(1)(i) = '1' then
						portA(i) <= spetial_func_regs(2)(i);
					else 
						portA(i) <= 'Z';
					end if;
					spetial_func_regs(0)(i) <= portA(i);
					if spetial_func_regs(4)(i) = '1' then
						portB(i) <= spetial_func_regs(5)(i);
					else 
						portC(i) <= 'Z';
					end if;
					spetial_func_regs(3)(i) <= portB(i);
					if spetial_func_regs(7)(i) = '1' then
						portC(i) <= spetial_func_regs(8)(i);
					else 
						portC(i) <= 'Z';
					end if;
					spetial_func_regs(6)(i) <= portC(i);
					if spetial_func_regs(10)(i) = '1' then
						portD(i) <= spetial_func_regs(11)(i);
					else 
						portD(i) <= 'Z';
					end if;
					spetial_func_regs(9)(i) <= portD(i);
				end loop;
				case state is
				when fetch =>
					state <= decode;
					insts <= inst_mem_data_msb & inst_mem_data_lsb;
				when decode =>
					if(call_jmp_32bit_flag = '0') then
						if(insts(15 downto 10) = "000011") then--add
							dest_reg_no := to_integer(unsigned(insts(8 downto 4)));
							src_reg_no := to_integer(unsigned(insts(9) & insts(3 downto 0)));
							dest_reg(7 downto 0) := genpurp_regs(dest_reg_no);
							src_reg(7 downto 0) := genpurp_regs(src_reg_no);
							result(7 downto 0) := std_logic_vector(unsigned(src_reg(7 downto 0)) + to_integer(unsigned(genpurp_regs(src_reg_no)))); 
						
							C_flag <= ((dest_reg(7) and genpurp_regs(src_reg_no)(7)) or (genpurp_regs(src_reg_no)(7) and (not result(7)))) or ((not result(7)) and dest_reg(7));
							if(result(7 downto 0) = "00000000") then
								Z_flag <= '1';
							else
								Z_flag <= '0';
							end if;
							N := result(7);
							V :=  ( (dest_reg(7) and genpurp_regs(src_reg_no)(7)) and (not result(7)) ) or ( ( (not dest_reg(7)) and (not genpurp_regs(src_reg_no)(7)) ) and result(7) );
							N_flag <= N;
							V_flag <= V;
							S_flag <= N xor V;
							H_flag <= ((dest_reg(3) and genpurp_regs(src_reg_no)(3)) or (genpurp_regs(src_reg_no)(3) and (not result(3)))) or ((not result(3)) and dest_reg(3));
							genpurp_regs(dest_reg_no) <= result(7 downto 0);
							pc <= pc + 1;
							reg_inst <= add;
						elsif(insts(15 downto 10) = "000111")	then--adc
							dest_reg_no := to_integer(unsigned(insts(8 downto 4)));
							src_reg_no := to_integer(unsigned(insts(9) & insts(3 downto 0)));
							dest_reg(7 downto 0) := genpurp_regs(dest_reg_no);
							src_reg(7 downto 0) := genpurp_regs(src_reg_no);
							result(7 downto 0) := std_logic_vector(unsigned(src_reg(7 downto 0)) + to_integer(unsigned(genpurp_regs(src_reg_no)))); 
							
							r7 := result(7);
							rd7 := dest_reg(7);
							rr7 := genpurp_regs(src_reg_no)(7);
							r3 := result(3);
							rd3 := dest_reg(3);
							rr3 := genpurp_regs(src_reg_no)(3);
							
							C_flag <= (rd7 and rr7) or ((rr7 and (not r7)) or ((not r7) and rd7));
							if(result(7 downto 0) = "00000000") then
								Z_flag <= '1';
							else
								Z_flag <= '0';
							end if;
							N := result(7);
							V :=  ((rd7 and rr7) and (not r7) ) or (((not rd7) and (not rr7)) and r7);
							N_flag <= N;
							V_flag <= V;
							S_flag <= N xor V;
							H_flag <= (rd3 and rr3) or ((rr3 and (not r3)) or ((not r3) and rd3)); 
							genpurp_regs(dest_reg_no) <= result(7 downto 0);
							pc <= pc + 1;
							reg_inst <= adc;
						elsif (insts(15 downto 8) = "10010110") then--adiw
							dest_reg(7 downto 0) := genpurp_regs(24 + to_integer(unsigned(insts(5 downto 4) & '0')));
							dest_reg(15 downto 8) := genpurp_regs(24 + to_integer(unsigned(insts(5 downto 4) & '1')));
							immidiate := "0000000000" & insts(7 downto 6) & insts(3 downto 0);
							reg_16buf := std_logic_vector(unsigned(dest_reg) + to_integer(unsigned(immidiate)));
							genpurp_regs(to_integer(unsigned(insts(5 downto 4) & '1'))+24) <= reg_16buf(15 downto 8);
							genpurp_regs(to_integer(unsigned(insts(5 downto 4) & '0'))+24) <= reg_16buf(7 downto 0);
							C_flag <= genpurp_regs(to_integer(unsigned(insts(5 downto 4) & '1'))+24)(7) and (not reg_16buf(15));
							if(reg_16buf = "0000000000000000") then
								Z_flag <= '1';
							else
								Z_flag <= '0';
							end if;
							N := reg_16buf(15);
							V := (not genpurp_regs(to_integer(unsigned(insts(5 downto 4) & '1'))+24)(7)) and reg_16buf(15);
							N_flag <= N;
							V_flag <= V;
							S_flag <= N xor V;
							pc <= pc + 1;
							reg_inst <= adiw;
						elsif(insts(15 downto 10) = "001000")	then--and
							dest_reg_no := to_integer(unsigned(insts(8 downto 4)));
							src_reg_no := to_integer(unsigned(insts(9) & insts(3 downto 0)));
							dest_reg(7 downto 0) := genpurp_regs(dest_reg_no);
							src_reg(7 downto 0) := genpurp_regs(src_reg_no);
							result(7 downto 0) := dest_reg(7 downto 0) and src_reg(7 downto 0); 
							
							r7 := result(7);
							rd7 := dest_reg(7);
							rr7 := genpurp_regs(src_reg_no)(7);
							r3 := result(3);
							rd3 := dest_reg(3);
							rr3 := genpurp_regs(src_reg_no)(3);
							
							if(result(7 downto 0) = "00000000") then
								Z_flag <= '1';
							else
								Z_flag <= '0';
							end if;
							N := r7;
							V :=  '0';
							N_flag <= N;
							V_flag <= V;
							S_flag <= N xor V;
							genpurp_regs(dest_reg_no) <= result(7 downto 0);
							pc <= pc + 1;
							reg_inst <= and0;
						elsif(insts(15 downto 12) = "0111")	then--andi
							dest_reg_no := to_integer(unsigned(insts(7 downto 4)));
							immidiate(7 downto 0) := insts(11 downto 8) & insts(3 downto 0);
							dest_reg(7 downto 0) := genpurp_regs(dest_reg_no);
							result(7 downto 0) := dest_reg(7 downto 0) and immidiate(7 downto 0); 
							
							r7 := result(7);
							rd7 := dest_reg(7);
							rr7 := genpurp_regs(src_reg_no)(7);
							r3 := result(3);
							rd3 := dest_reg(3);
							rr3 := genpurp_regs(src_reg_no)(3);
							
							if(result(7 downto 0) = "00000000") then
								Z_flag <= '1';
							else
								Z_flag <= '0';
							end if;
							N := r7;
							V :=  '0';
							N_flag <= N;
							V_flag <= V;
							S_flag <= N xor V;
							genpurp_regs(dest_reg_no) <= result(7 downto 0);
							pc <= pc + 1;
							reg_inst <= andi;
						elsif(insts(15 downto 12) = "0111")	then--asr
							dest_reg_no := to_integer(unsigned(insts(7 downto 4)));
							dest_reg(7 downto 0) := genpurp_regs(dest_reg_no);
							result(7 downto 0) := dest_reg(7) & dest_reg(7 downto 1); 
							
							r7 := result(7);
							rd7 := dest_reg(7);
							rr7 := genpurp_regs(src_reg_no)(7);
							r3 := result(3);
							rd3 := dest_reg(3);
							rr3 := genpurp_regs(src_reg_no)(3);
							
							if(result(7 downto 0) = "00000000") then
								Z_flag <= '1';
							else
								Z_flag <= '0';
							end if;
							C := dest_reg(0);
							N := r7;
							V :=  '0';
							N_flag <= N;
							V_flag <= N xor C;
							S_flag <= N xor V;
							genpurp_regs(dest_reg_no) <= result(7 downto 0);
							pc <= pc + 1;
							reg_inst <= asr;
						elsif(insts(15 downto 7) = "100101001" and insts(3 downto 0) = "1000")	then--bclr
							dest_reg_no := to_integer(unsigned(insts(6 downto 4)));
							sreg(dest_reg_no) <= '0';
							pc <= pc + 1;
							reg_inst <= bclr;
							
						elsif(insts(15 downto 9) = "1111100" and insts(3) = '0')	then--bld
							dest_reg_no := to_integer(unsigned(insts(8 downto 4)));
							src_reg_no := to_integer(unsigned(insts(2 downto 0)));
							genpurp_regs(dest_reg_no)(src_reg_no) <= T_flag;
							pc <= pc + 1;
							reg_inst <= bld;
							
						elsif(insts(15 downto 10) = "111101") then --brbc
							src_reg_no := to_integer(unsigned(insts(2 downto 0)));
							if(sreg(src_reg_no) = '0') then
								immidiate := insts(9) & insts(9) & insts(9) & insts(9) & insts(9) & insts(9) & insts(9) & insts(9) & insts(9) & insts(9 downto 3);
								pc <= to_integer(1 + (pc + signed(immidiate)));
							else
								pc <= pc + 1;
							end if;
							reg_inst <= brbc;
							
						elsif(insts(15 downto 10) = "111100") then --brbs
							src_reg_no := to_integer(unsigned(insts(2 downto 0)));
							if(sreg(src_reg_no) = '1') then
								immidiate := insts(9) & insts(9) & insts(9) & insts(9) & insts(9) & insts(9) & insts(9) & insts(9) & insts(9) & insts(9 downto 3);
								pc <= to_integer(1 + (pc + signed(immidiate)));
							else
								pc <= pc + 1;
							end if;
							reg_inst <= brbs;
							
						elsif(insts(15 downto 10) = "111101" and insts(2 downto 0) = "000") then --brcc
							src_reg_no := to_integer(unsigned(insts(2 downto 0)));
							if(sreg(0) = '0') then
								immidiate := insts(9) & insts(9) & insts(9) & insts(9) & insts(9) & insts(9) & insts(9) & insts(9) & insts(9) & insts(9 downto 3);
								pc <= to_integer(1 + (pc + signed(immidiate)));
							else
								pc <= pc + 1;
							end if;
							reg_inst <= brcc;
							
						elsif(insts(15 downto 10) = "111100" and insts(2 downto 0) = "000") then --brcs
							src_reg_no := to_integer(unsigned(insts(2 downto 0)));
							if(sreg(0) = '1') then
								immidiate := insts(9) & insts(9) & insts(9) & insts(9) & insts(9) & insts(9) & insts(9) & insts(9) & insts(9) & insts(9 downto 3);
								pc <= to_integer(1 + (pc + signed(immidiate)));
							else
								pc <= pc + 1;
							end if;
							reg_inst <= brcs;						

						elsif(insts(15 downto 10) = "111100" and insts(2 downto 0) = "001") then --breq
							if(sreg(1) = '1') then
								immidiate := insts(9) & insts(9) & insts(9) & insts(9) & insts(9) &insts(9) & insts(9) & insts(9) & insts(9) & insts(9 downto 3);
								pc <= to_integer(1 + (pc + signed(immidiate)));
							else
								pc <= pc + 1;
							end if;
							reg_inst <= breq;	
		
						elsif(insts(15 downto 10) = "111101" and insts(2 downto 0) = "100") then --brge
							if((sreg(2) xor sreg(3)) = '0') then
								immidiate := insts(9) & insts(9) & insts(9) & insts(9) & insts(9) &insts(9) & insts(9) & insts(9) & insts(9) & insts(9 downto 3);
								pc <= to_integer(1 + (pc + signed(immidiate)));
							else
								pc <= pc + 1;
							end if;
							reg_inst <= brge;
						
						elsif(insts(15 downto 10) = "111101" and insts(2 downto 0) = "101") then --brhc
							if(sreg(5) = '0') then
								immidiate := insts(9) & insts(9) & insts(9) & insts(9) & insts(9) &insts(9) & insts(9) & insts(9) & insts(9) & insts(9 downto 3);
								pc <= to_integer(1 + (pc + signed(immidiate)));
							else
								pc <= pc + 1;
							end if;
							reg_inst <= brhc;		
							
						elsif(insts(15 downto 10) = "111100" and insts(2 downto 0) = "101") then --brhs
							if(sreg(5) = '1') then
								immidiate := insts(9) & insts(9) & insts(9) & insts(9) & insts(9) &insts(9) & insts(9) & insts(9) & insts(9) & insts(9 downto 3);
								pc <= to_integer(1 + (pc + signed(immidiate)));
							else
								pc <= pc + 1;
							end if;
							reg_inst <= brhs;	
							
						elsif(insts(15 downto 10) = "111101" and insts(2 downto 0) = "111") then --brid
							if(sreg(7) = '1') then
								immidiate := insts(9) & insts(9) & insts(9) & insts(9) & insts(9) &insts(9) & insts(9) & insts(9) & insts(9) & insts(9 downto 3);
								pc <= to_integer(1 + (pc + signed(immidiate)));
							else
								pc <= pc + 1;
							end if;
							reg_inst <= brid;	
							
						elsif(insts(15 downto 10) = "111100" and insts(2 downto 0) = "111") then --brie
							if(sreg(7) = '1') then
								immidiate := insts(9) & insts(9) & insts(9) & insts(9) & insts(9) &insts(9) & insts(9) & insts(9) & insts(9) & insts(9 downto 3);
								pc <= to_integer(1 + (pc + signed(immidiate)));
							else
								pc <= pc + 1;
							end if;
							reg_inst <= brie;	
							
						elsif(insts(15 downto 10) = "111100" and insts(2 downto 0) = "000") then --brlo
							if(sreg(0) = '1') then
								immidiate := insts(9) & insts(9) & insts(9) & insts(9) & insts(9) &insts(9) & insts(9) & insts(9) & insts(9) & insts(9 downto 3);
								pc <= to_integer(1 + (pc + signed(immidiate)));
							else
								pc <= pc + 1;
							end if;
							reg_inst <= brlo;	
						
						elsif(insts(15 downto 10) = "111100" and insts(2 downto 0) = "100") then --brlt
							if((sreg(2) xor sreg(3)) = '1') then
								immidiate := insts(9) & insts(9) & insts(9) & insts(9) & insts(9) &insts(9) & insts(9) & insts(9) & insts(9) & insts(9 downto 3);
								pc <= to_integer(1 + (pc + signed(immidiate)));
							else
								pc <= pc + 1;
							end if;
							reg_inst <= brlt;
							
						elsif(insts(15 downto 10) = "111100" and insts(2 downto 0) = "010") then --brmi
							if(sreg(2) = '1') then
								immidiate := insts(9) & insts(9) & insts(9) & insts(9) & insts(9) &insts(9) & insts(9) & insts(9) & insts(9) & insts(9 downto 3);
								pc <= to_integer(1 + (pc + signed(immidiate)));
							else
								pc <= pc + 1;
							end if;
							reg_inst <= brmi;

						elsif(insts(15 downto 10) = "111101" and insts(2 downto 0) = "001") then --brne
							if(sreg(1) = '0') then
								immidiate := insts(9) & insts(9) & insts(9) & insts(9) & insts(9) &insts(9) & insts(9) & insts(9) & insts(9) & insts(9 downto 3);
								pc <= to_integer(1 + (pc + signed(immidiate)));
							else
								pc <= pc + 1;
							end if;
							reg_inst <= brne;

						
						elsif(insts(15 downto 10) = "111101" and insts(2 downto 0) = "010") then --brpl
							if(sreg(2) = '0') then
								immidiate := insts(9) & insts(9) & insts(9) & insts(9) & insts(9) &insts(9) & insts(9) & insts(9) & insts(9) & insts(9 downto 3);
								pc <= to_integer(1 + (pc + signed(immidiate)));
							else
								pc <= pc + 1;
							end if;
							reg_inst <= brpl;

							
						elsif(insts(15 downto 10) = "111101" and insts(2 downto 0) = "000") then --brsh
							if(sreg(0) = '0') then
								immidiate := insts(9) & insts(9) & insts(9) & insts(9) & insts(9) &insts(9) & insts(9) & insts(9) & insts(9) & insts(9 downto 3);
								pc <= to_integer(1 + (pc + signed(immidiate)));
							else
								pc <= pc + 1;
							end if;
							reg_inst <= brsh;

						elsif(insts(15 downto 10) = "111101" and insts(2 downto 0) = "110") then --brtc
							if(sreg(6) = '0') then
								immidiate := insts(9) & insts(9) & insts(9) & insts(9) & insts(9) &insts(9) & insts(9) & insts(9) & insts(9) & insts(9 downto 3);
								pc <= to_integer(1 + (pc + signed(immidiate)));
							else
								pc <= pc + 1;
							end if;
							reg_inst <= brtc;

						elsif(insts(15 downto 10) = "111100" and insts(2 downto 0) = "110") then --brts
							if(sreg(6) = '1') then
								immidiate := insts(9) & insts(9) & insts(9) & insts(9) & insts(9) &insts(9) & insts(9) & insts(9) & insts(9) & insts(9 downto 3);
								pc <= to_integer(1 + (pc + signed(immidiate)));
							else
								pc <= pc + 1;
							end if;
							reg_inst <= brts;

						elsif(insts(15 downto 10) = "111101" and insts(2 downto 0) = "011") then --brvc
							if(sreg(3) = '0') then
								immidiate := insts(9) & insts(9) & insts(9) & insts(9) & insts(9) &insts(9) & insts(9) & insts(9) & insts(9) & insts(9 downto 3);
								pc <= to_integer(1 + (pc + signed(immidiate)));
							else
								pc <= pc + 1;
							end if;
							reg_inst <= brvc;

						elsif(insts(15 downto 10) = "111100" and insts(2 downto 0) = "011") then --brvs
							if(sreg(3) = '1') then
								immidiate := insts(9) & insts(9) & insts(9) & insts(9) & insts(9) &insts(9) & insts(9) & insts(9) & insts(9) & insts(9 downto 3);
								pc <= to_integer(1 + (pc + signed(immidiate)));
							else
								pc <= pc + 1;
							end if;
							reg_inst <= brvs;	

						elsif(insts(15 downto 7) = "100101000" and insts(3 downto 0) = "1000")	then--bset
							dest_reg_no := to_integer(unsigned(insts(6 downto 4)));
							sreg(dest_reg_no) <= '1';
							pc <= pc + 1;
							reg_inst <= bset;
							
						elsif(insts(15 downto 9) = "1111100" and insts(3) = '0')	then--bst
							dest_reg_no := to_integer(unsigned(insts(8 downto 4)));
							src_reg_no := to_integer(unsigned(insts(2 downto 0)));
							T_flag <= genpurp_regs(dest_reg_no)(src_reg_no);
							pc <= pc + 1;
							reg_inst <= bst;
						
						elsif(insts(15 downto 9) = "1001010" and insts(3 downto 1) = "111") then --call
							call_jmp_32bit_flag <= '1';
							call_jmp_pc_tmp(21 downto 17) <= insts(8 downto 4);	
							reg_inst <= call;
							pc <= pc + 1;
							
						elsif(insts(15 downto 8) = "10011000") then --cbi
							bit_no := to_integer(unsigned(insts(2 downto 0)));
							io_reg_no := to_integer(unsigned(insts(7 downto 3)));
							spetial_func_regs(io_reg_no)(bit_no) <= '0';
							pc <= pc + 1;
							reg_inst <= cbi;	
							
						elsif(insts = "1001010010001000") then --clc
							C_flag <= '0';
							pc <= pc + 1;
							reg_inst <= clc;
							
						elsif(insts = "1001010011011000") then --clh
							H_flag <= '0';
							pc <= pc + 1;
							reg_inst <= clh;
							
						elsif(insts = "1001010011111000") then --cli
							I_flag <= '0';
							pc <= pc + 1;
							reg_inst <= cli;
							
						elsif(insts = "1001010010101000") then --cln
							N_flag <= '0';
							pc <= pc + 1;
							reg_inst <= cln;

						elsif(insts = "1001010011001000") then --cls
							S_flag <= '0';
							pc <= pc + 1;
							reg_inst <= cls;
							
						elsif(insts = "1001010011101000") then --clt
							T_flag <= '0';
							pc <= pc + 1;
							reg_inst <= clt;
							
						elsif(insts = "1001010010111000") then --clv
							V_flag <= '0';
							pc <= pc + 1;
							reg_inst <= clv;
							
						elsif(insts = "1001010010011000") then --clz
							Z_flag <= '0';
							pc <= pc + 1;
							reg_inst <= clv;
					  elsif (insts(15 downto 9) = "1001010" and insts(3 downto 0) = "0000") then	--com
							dest_reg_no := to_integer(unsigned(insts(8 downto 4)));
							dest_reg(7 downto 0) := genpurp_regs(dest_reg_no);
							result(7 downto 0) := std_logic_vector(255-unsigned(genpurp_regs(dest_reg_no)));
							
							C_flag <= '1';
							if(result(7 downto 0) = "00000000") then
								Z_flag <= '1';
							else
								Z_flag <= '0';
							end if;
							N := result(7);
							V := '0';
							N_flag <= N;
							V_flag <= V;
							S_flag <= N xor V;
							genpurp_regs(dest_reg_no) <= result(7 downto 0);
							pc <= pc + 1;
							reg_inst <= com;	
							
						elsif (insts(15 downto 10) = "000101") then --cp
							dest_reg_no := to_integer(unsigned(insts(8 downto 4)));
							src_reg_no := to_integer(unsigned(insts(9) & insts(3 downto 0)));
							pc <= pc + 1;
							reg_inst <= cp;
							src_reg(7 downto 0) := genpurp_regs(src_reg_no);
							dest_reg(7 downto 0) := genpurp_regs(dest_reg_no);
							result(7 downto 0) := std_logic_vector(unsigned(genpurp_regs(dest_reg_no)) - to_integer(unsigned(genpurp_regs(src_reg_no))));
							
							r7 := result(7);
							rd7 := dest_reg(7);
							rr7 := src_reg(7);
							r3 := result(3);
							rd3 := dest_reg(3);
							rr3 := src_reg(3);
							
							C_flag <= (((not rd7) and rr7) or (rr7 and r7)) or (r7 and (not rd7));
							if(result(7 downto 0) = "00000000") then
								Z_flag <= '1';
							else
								Z_flag <= '0';
							end if;
							N := result(7);
							V := (rd7 and ((not rr7) and (not r7))) or ((not rd7) and (rr7 and r7));
							N_flag <= N;
							V_flag <= V;
							S_flag <= N xor V;
							H_flag <= (((not rd3) and rr3) or (rr3 and r3)) or (r3 and (not rd3));

						elsif (insts(15 downto 10) = "000101") then --cpc
							dest_reg_no := to_integer(unsigned(insts(8 downto 4)));
							src_reg_no := to_integer(unsigned(insts(9) & insts(3 downto 0)));
							pc <= pc + 1;
							reg_inst <= cpc;
							src_reg(7 downto 0) := genpurp_regs(src_reg_no);
							dest_reg(7 downto 0) := genpurp_regs(dest_reg_no);
							result(7 downto 0) := std_logic_vector(unsigned(genpurp_regs(dest_reg_no)) - to_integer(unsigned(genpurp_regs(src_reg_no))));
							
							r7 := result(7);
							rd7 := dest_reg(7);
							rr7 := src_reg(7);
							r3 := result(3);
							rd3 := dest_reg(3);
							rr3 := src_reg(3);
							
							C_flag <= ((not rd7) and rr7) or ((rr7 and r7) or (r7 and (not rd7)));
							if(result(7 downto 0) = "00000000") then
								if(sreg(1) = '1') then
									Z_flag <= '1';
								else
									Z_flag <= '0';
								end if;
							else
								Z_flag <= '0';
							end if;
							N := result(7);
							V := (rd7 and ((not rr7) and (not r7))) or ((not rd7) and (rr7 and r7));
							N_flag <= N;
							V_flag <= V;
							S_flag <= N xor V;
							H_flag <= (((not rd3) and rr3) or (rr3 and r3)) or (r3 and (not rd3));
		 				
						elsif (insts(15 downto 12) = "0011") then --cpi
							dest_reg_no := to_integer(unsigned(insts(7 downto 4)));
							immidiate(7 downto 0) := insts(11 downto 8) & insts(3 downto 0);
							pc <= pc + 1;
							reg_inst <= cpi;
							src_reg(7 downto 0) := genpurp_regs(src_reg_no);
							dest_reg(7 downto 0) := genpurp_regs(dest_reg_no);
							result(7 downto 0) := std_logic_vector(unsigned(genpurp_regs(dest_reg_no)) - to_integer(unsigned(genpurp_regs(src_reg_no))));
							
							r7 := result(7);
							rd7 := dest_reg(7);
							rr7 := immidiate(7);
							r3 := result(3);
							rd3 := dest_reg(3);
							rr3 := immidiate(3);
							
							C_flag <= (((not rd7) and rr7) or (rr7 and r7)) or (r7 and (not rd7));
							if(result(7 downto 0) = "00000000") then
								Z_flag <= '1';
							else
								Z_flag <= '0';
							end if;
							N := result(7);
							V := (rd7 and ((not rr7) and (not r7))) or ((not rd7) and (rr7 and r7));
							N_flag <= N;
							V_flag <= V;
							S_flag <= N xor V;
							H_flag <= (((not rd3) and rr3) or (rr3 and r3)) or (r3 and (not rd3));			
						
						elsif (insts(15 downto 10) = "000101") then --cpse
							dest_reg_no := to_integer(unsigned(insts(8 downto 4)));
							src_reg_no := to_integer(unsigned(insts(9) & insts(3 downto 0)));
							
							reg_inst <= cpse;

							if(genpurp_regs(dest_reg_no) = genpurp_regs(src_reg_no)) then
								pc <= pc + 2;
							else
								pc <= pc + 1;
							end if;
						
						elsif (insts(15 downto 9) = "1001010" and insts(3 downto 0) = "1010") then -- dec
							dest_reg_no := to_integer(unsigned(insts(8 downto 4)));
							dest_reg(7 downto 0) := genpurp_regs(dest_reg_no);
							reg_inst <= dec;

							dest_reg(7 downto 0) := genpurp_regs(dest_reg_no);
							result(7 downto 0) := std_logic_vector(unsigned(genpurp_regs(dest_reg_no)) - 1);
							
							r7 := result(7);
							rd7 := dest_reg(7);
							rr7 := immidiate(7);
							r3 := result(3);
							rd3 := dest_reg(3);
							rr3 := immidiate(3);
							
							C_flag <= (((not rd7) and rr7) or (rr7 and r7)) or (r7 and (not rd7));
							if(result(7 downto 0) = "00000000") then
								Z_flag <= '1';
							else
								Z_flag <= '0';
							end if;
							N := result(7);
							V := (((not result(7)) and result(6)) and (result(5) and result(4))) and ((result(3) and result(2)) and (result(1) and result(0)));
							N_flag <= N;
							V_flag <= V;
							S_flag <= N xor V;
							
							genpurp_regs(dest_reg_no) <= result(7 downto 0);
							pc <= pc + 1;
							
						elsif(insts(15 downto 0) = "1001010100001001") then --icall	
							pc <= to_integer(unsigned(genpurp_regs(31)) & unsigned(genpurp_regs(30)));
							ram_data_tmp := std_logic_vector(to_unsigned(pc+1,16));
							ram_addr_lsb <= std_logic_vector(to_unsigned((stack_pointer - 2),16));
							ram_in_lsb <= ram_data_tmp(7 downto 0);
							ram_wr_lsb <= '1';
							ram_addr_msb <= std_logic_vector(to_unsigned((stack_pointer - 1),16));
							ram_in_msb <= ram_data_tmp(15 downto 8);
							ram_wr_msb <= '1';
							stack_pointer <= stack_pointer - 2;
							reg_inst <= icall;
						elsif(insts(15 downto 0) = "1001010000001001") then --ijmp
							pc <= to_integer(unsigned(genpurp_regs(31)) & unsigned(genpurp_regs(30)));
							reg_inst <= ijmp;
							
						elsif(insts(15 downto 11) = "10110") then --in
							dest_reg_no := to_integer(unsigned(insts(8 downto 4)));
							io_reg_no := to_integer(unsigned(insts(10 downto 9)) & unsigned(insts(3 downto 0)));
							genpurp_regs(dest_reg_no) <= spetial_func_regs(io_reg_no);
							pc <= pc + 1;
							reg_inst <= in0;
						
						
						elsif (insts(15 downto 9) = "1001010" and insts(3 downto 0) = "0011") then -- inc
							dest_reg_no := to_integer(unsigned(insts(8 downto 4)));
							dest_reg(7 downto 0) := genpurp_regs(dest_reg_no);
							reg_inst <= inc;

							dest_reg(7 downto 0) := genpurp_regs(dest_reg_no);
							result(7 downto 0) := std_logic_vector(unsigned(genpurp_regs(dest_reg_no)) + 1);
							
							r7 := result(7);
							rd7 := dest_reg(7);
							rr7 := immidiate(7);
							r3 := result(3);
							rd3 := dest_reg(3);
							rr3 := immidiate(3);
							
							if(result(7 downto 0) = "00000000") then
								Z_flag <= '1';
							else
								Z_flag <= '0';
							end if;
							N := result(7);
							V :=  (not (((not result(7)) or result(6)) or (result(5) or result(4))) or ((result(3) or result(2)) or (result(1) or result(0))));
							N_flag <= N;
							V_flag <= V;
							S_flag <= N xor V;
							
							genpurp_regs(dest_reg_no) <= result(7 downto 0);
							pc <= pc + 1;
							
						elsif(insts(15 downto 9) = "1001010" and insts(3 downto 1) = "110") then --jmp
							call_jmp_32bit_flag <= '1';
							call_jmp_pc_tmp(21 downto 17) <= insts(8 downto 4);	
							reg_inst <= jmp;
							pc <= pc + 1;
						
						elsif(insts(15 downto 9) = "1001001" and insts(3 downto 0) = "0110") then --lac
							dest_reg_no := to_integer(unsigned(insts(8 downto 4)));
							ram_addr_lsb <= std_logic_vector(unsigned(genpurp_regs(31)))&std_logic_vector(unsigned(genpurp_regs(31)));
							ram_addr_msb <= (others => '0') ;
							
							reg_inst <= lac;
							pc <= pc + 1;
						
						elsif(insts(15 downto 9) = "1001001" and insts(3 downto 0) = "0101") then --las
							dest_reg_no := to_integer(unsigned(insts(8 downto 4)));
							ram_addr_lsb <= std_logic_vector(unsigned(genpurp_regs(31)))&std_logic_vector(unsigned(genpurp_regs(31)));
							ram_addr_msb <= (others => '0') ;
							reg_inst <= las;
							pc <= pc + 1;
							
						elsif(insts(15 downto 9) = "1001001" and insts(3 downto 0) = "0101") then --lat
							dest_reg_no := to_integer(unsigned(insts(8 downto 4)));
							ram_addr_lsb <= std_logic_vector(unsigned(genpurp_regs(31)))&std_logic_vector(unsigned(genpurp_regs(31)));
							ram_addr_msb <= (others => '0') ;
							reg_inst <= lat;
							pc <= pc + 1;
							
						elsif(insts(15 downto 9) = "1001000" and insts(3 downto 0) = "1100") then --ld Rd,X
							dest_reg_no := to_integer(unsigned(insts(8 downto 4)));
							ram_addr_lsb <= std_logic_vector(unsigned(genpurp_regs(31)))&std_logic_vector(unsigned(genpurp_regs(31)));
							ram_addr_msb <= (others => '0') ;
							reg_inst <= ldX;
							pc <= pc + 1;
							
						elsif(insts(15 downto 9) = "1001000" and insts(3 downto 0) = "1101") then --ld Rd,X+
							dest_reg_no := to_integer(unsigned(insts(8 downto 4)));
							ram_addr_lsb <= std_logic_vector(unsigned(genpurp_regs(31)))&std_logic_vector(unsigned(genpurp_regs(31)));
							ram_addr_msb <= (others => '0') ;
							reg_inst <= ldXp;
							pc <= pc + 1;
							
						elsif(insts(15 downto 9) = "1001000" and insts(3 downto 0) = "1110") then --ld Rd,-X
							dest_reg_no := to_integer(unsigned(insts(8 downto 4)));
							tmp16 := std_logic_vector(unsigned(genpurp_regs(27)) & unsigned(genpurp_regs(26))); 
							tmp16 := std_logic_vector(unsigned(tmp16) - 1);
							genpurp_regs(27) <= tmp16(15 downto 8);
							genpurp_regs(26) <= tmp16(7 downto 0);
							
							ram_addr_lsb <= std_logic_vector(unsigned(tmp16));
							ram_addr_msb <= (others => '0');
							reg_inst <= ldmX;
							pc <= pc + 1;
							
						elsif (insts(15 downto 10) = "000110") then --sub
							dest_reg_no := to_integer(unsigned(insts(8 downto 4)));
							src_reg_no := to_integer(unsigned(insts(9) & insts(3 downto 0)));
							pc <= pc + 1;
							reg_inst <= sub;
							dest_reg(7 downto 0) := genpurp_regs(dest_reg_no);
							result(7 downto 0) := std_logic_vector(unsigned(genpurp_regs(dest_reg_no)) - to_integer(unsigned(genpurp_regs(src_reg_no))));
							C_flag <= (((not dest_reg(7)) and genpurp_regs(src_reg_no)(7)) or (genpurp_regs(src_reg_no)(7) and result(7))) or (result(7) and (not dest_reg(7)));
							if(result(7 downto 0) = "00000000") then
								Z_flag <= '1';
							else
								Z_flag <= '0';
							end if;
							N := result(7);
							V := ( (dest_reg(7) and genpurp_regs(src_reg_no)(7)) and (not result(7)) ) or ( ( (not dest_reg(7)) and (not genpurp_regs(src_reg_no)(7)) ) and result(7));
							N_flag <= N;
							V_flag <= V;
							S_flag <= N xor V;
							H_flag <= (((not dest_reg(3)) and genpurp_regs(src_reg_no)(3)) or (genpurp_regs(src_reg_no)(3) and result(3))) or (result(3) and (not dest_reg(3)));
							genpurp_regs(dest_reg_no) <= result(7 downto 0);
		 
						elsif(insts(15 downto 10) = "100111" ) then --mul
							dest_reg_no := to_integer(unsigned(insts(8 downto 4))); 
							src_reg_no := to_integer(unsigned(insts(9) & insts(3 downto 0)));
							multa <= genpurp_regs(dest_reg_no);
							multb <= genpurp_regs(to_integer(unsigned(insts(9) & insts(3 downto 0))));
							pc <= pc + 1;
							reg_inst <= mul;
						elsif(insts(15 downto 12) = "1110") then --ldi
							genpurp_regs(to_integer(unsigned(('0' & insts(7 downto 4))) + 16)) <= insts(11 downto 8) & insts(3 downto 0);
							pc <= pc + 1;
							reg_inst <= ldi;

						elsif(insts(15 downto 11) = "10111") then --out
							src_reg_no := to_integer(unsigned(insts(8 downto 4)));
							io_reg_no := to_integer(unsigned(insts(10 downto 9)) & unsigned(insts(3 downto 0)));
							spetial_func_regs(io_reg_no) <= genpurp_regs(src_reg_no);
							pc <= pc + 1;
							reg_inst <= out0;
						elsif(insts(15 downto 12) = "1100") then --rjmp
							immidiate := insts(11) & insts(11)& insts(11) & insts(11) & insts(11 downto 0);
							pc <= to_integer(1 + (pc + signed(immidiate)));
							reg_inst <= rjmp;
						
						elsif(insts = "1001010100001000") then --ret
							ram_addr_lsb <= std_logic_vector(to_unsigned(stack_pointer + 2,16));
							ram_addr_msb <= std_logic_vector(to_unsigned(stack_pointer + 3,16));
							pc_to_inst_wait <= 0;
							reg_inst <= ret;
						elsif(insts = "0000000000000000") then  --nop
							null;
						else
							null;
						end if;
					elsif(call_jmp_32bit_flag = '1') then
						if(reg_inst = call) then
							pc <= to_integer(unsigned(insts));
							ram_data_tmp := std_logic_vector(to_unsigned(pc+1,16));
							ram_addr_lsb <= std_logic_vector(to_unsigned((stack_pointer - 2),16));
							ram_in_lsb <= ram_data_tmp(7 downto 0);
							ram_wr_lsb <= '1';
							ram_addr_msb <= std_logic_vector(to_unsigned((stack_pointer - 1),16));
							ram_in_msb <= ram_data_tmp(15 downto 8);
							ram_wr_msb <= '1';
							stack_pointer <= stack_pointer - 2;
						
						elsif(reg_inst = jmp) then
							pc <= to_integer(unsigned(insts));
						end if;
						call_jmp_32bit_flag <= '0';
					end if;
					state <= execute;
				when execute =>
					ram_wr_lsb <= '0';
					ram_wr_msb <= '0';
					if(reg_inst = mul) then
						genpurp_regs(1) <= mult_out(15 downto 8);
						genpurp_regs(0) <= mult_out(7 downto 0);
						state <= fetch;
					elsif (reg_inst = ret) then
						if(pc_to_inst_wait = 1) then
							state <= fetch;
						elsif (pc_to_inst_wait = 0) then
							pc <= to_integer(unsigned(ram_out_msb) & unsigned(ram_out_lsb));
							stack_pointer <= stack_pointer + 2;
							pc_to_inst_wait <= pc_to_inst_wait + 1;
						else
							pc_to_inst_wait <= pc_to_inst_wait + 1;
						end if;
					elsif(reg_inst = lac) then
						Z_reg := genpurp_regs(dest_reg_no) and std_logic_vector(X"FF" - (unsigned(ram_out_lsb))); 
						ram_in_lsb <= Z_reg(7 downto 0);
						ram_wr_lsb <= '1';
						state <= fetch;
					elsif(reg_inst = las) then
						Z_reg := genpurp_regs(dest_reg_no) or ram_out_lsb;
						ram_wr_lsb <= '1';
						state <= fetch;
					elsif(reg_inst = lat) then
						Z_reg := genpurp_regs(dest_reg_no) xor ram_out_lsb;
						ram_in_lsb <= Z_reg(7 downto 0);
						ram_wr_lsb <= '1';
						state <= fetch;
					elsif(reg_inst = ldX) then
						
						state <= fetch;
					else
						state <= fetch;
					end if;
					sreg <= I_flag & T_flag & H_flag & S_flag & V_flag & N_flag & Z_flag & C_flag;
				when others =>
					null;
				end case;
			end if;
		end if;
	end process;

end logic;

