set_global_assignment -entity "inst_memory" -library "lib_inst_memory" -name IP_TOOL_NAME "Qsys"
set_global_assignment -entity "inst_memory" -library "lib_inst_memory" -name IP_TOOL_VERSION "17.0"
set_global_assignment -entity "inst_memory" -library "lib_inst_memory" -name IP_TOOL_ENV "Qsys"
set_global_assignment -library "lib_inst_memory" -name SPD_FILE [file join $::quartus(sip_path) "../inst_memory.spd"]
set_global_assignment -library "lib_inst_memory" -name MISC_FILE [file join $::quartus(sip_path) "../../inst_memory.qsys"]

set_global_assignment -library "lib_inst_memory" -name MISC_FILE [file join $::quartus(sip_path) "inst_memory.vhd"]
set_global_assignment -library "lib_inst_memory" -name MISC_FILE [file join $::quartus(sip_path) "submodules/altera_onchip_flash_util.v"]
set_global_assignment -library "lib_inst_memory" -name MISC_FILE [file join $::quartus(sip_path) "submodules/altera_onchip_flash.v"]
set_global_assignment -library "lib_inst_memory" -name MISC_FILE [file join $::quartus(sip_path) "submodules/altera_onchip_flash_avmm_data_controller.v"]
set_global_assignment -library "lib_inst_memory" -name MISC_FILE [file join $::quartus(sip_path) "submodules/altera_onchip_flash_avmm_csr_controller.v"]
