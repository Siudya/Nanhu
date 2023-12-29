#***************************************************************************************
# Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
# Copyright (c) 2020-2021 Peng Cheng Laboratory
#
# XiangShan is licensed under Mulan PSL v2.
# You can use this software according to the terms and conditions of the Mulan PSL v2.
# You may obtain a copy of Mulan PSL v2 at:
#          http://license.coscl.org.cn/MulanPSL2
#
# THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
# EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
# MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
#
# See the Mulan PSL v2 for more details.
#***************************************************************************************

TOP = $(PREFIX)XSTop
SIM_TOP   = $(PREFIX)SimTop
FPGATOP = top.TopMain
BUILD_DIR ?= ./build

TOP_V = $(BUILD_DIR)/$(TOP).sv
SIM_TOP_V = $(BUILD_DIR)/$(SIM_TOP).sv

SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')
MEM_GEN = ./scripts/vlsi_mem_gen

SIMTOP  = top.SimTop
IMAGE  ?= temp
CONFIG ?= DefaultConfig
NUM_CORES ?= 1
ABS_WORK_DIR := $(shell pwd)
# VCS sim options
RUN_BIN_DIR ?= $(ABS_WORK_DIR)/ready-to-run
RUN_BIN ?= coremark-2-iteration
CONSIDER_FSDB ?= 1

ifdef FLASH
	RUN_OPTS := +flash=$(RUN_BIN_DIR)/$(RUN_BIN)
else
	RUN_OPTS := +workload=$(RUN_BIN_DIR)/$(RUN_BIN)
endif
ifeq ($(CONSIDER_FSDB),1)
	RUN_OPTS += +dump-wave=fsdb
endif
RUN_OPTS += +diff=$(ABS_WORK_DIR)/ready-to-run/riscv64-spike-so
#RUN_OPTS += +no-diff
RUN_OPTS += -fgp=num_threads:4,num_fsdb_threads:4
RUN_OPTS += -assert finish_maxfail=30 -assert global_finish_maxfail=10000
# co-simulation with DRAMsim3
ifeq ($(WITH_DRAMSIM3),1)
ifndef DRAMSIM3_HOME
$(error DRAMSIM3_HOME is not set)
endif
override SIM_ARGS += --with-dramsim3
endif

# top-down
ifeq ($(ENABLE_TOPDOWN),1)
override SIM_ARGS += --enable-topdown
endif

ifdef PREFIX
ARG_PREFIX = --prefix $(PREFIX)
else
ARG_PREFIX =
endif

# emu for the release version
RELEASE_ARGS = --fpga-platform $(ARG_PREFIX)
DEBUG_ARGS   = --enable-difftest $(ARG_PREFIX)

ifeq ($(RELEASE),1)
override SIM_ARGS += $(RELEASE_ARGS)
else
override SIM_ARGS += $(DEBUG_ARGS)
endif

.DEFAULT_GOAL = verilog

help:
	mill -i XiangShan.test.runMain $(SIMTOP) --xs-help

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	time -o $(@D)/time.log mill -i XiangShan.runMain $(FPGATOP) -td $(@D) \
		--config $(CONFIG) --full-stacktrace --num-cores $(NUM_CORES) \
		$(RELEASE_ARGS) --target systemverilog | tee build/make.log
ifeq ($(VCS), 1)
	@sed -i $$'s/$$fatal/assert(1\'b0)/g' $@
else
	@sed -i 's/$$fatal/xs_assert(`__LINE__)/g' $@
endif
	@python3 scripts/assertion_alter.py -o $@ $@
	@sed -i "s/assign \([a-zA-Z0-9_]\+\) = .* ? \(.*\) : [0-9]\+'bx;/assign \1 = \2;/g" $@
	@sed -i 's/_LOG_MODULE_PATH_/%m/g' $@
	@sed -i 's/\(\b[a-zA-Z_0-9]\+_[0-9]\+x[0-9]\+\b\)/$(PREFIX)\1/g' $@
	@sed -i '/\/\/ ----- 8< ----- FILE "firrtl_black_box_resource_files.f" ----- 8< -----/,$$d' $@
	@sed -i -e 's/\(peripheral\|memory\)_0_\(aw\|ar\|w\|r\|b\)_bits_/m_\1_\2_/g' \
	-e 's/\(dma\)_0_\(aw\|ar\|w\|r\|b\)_bits_/s_\1_\2_/g' $@
	@sed -i -e 's/\(peripheral\|memory\)_0_\(aw\|ar\|w\|r\|b\)_/m_\1_\2_/g' \
	-e 's/\(dma\)_0_\(aw\|ar\|w\|r\|b\)_\(ready\|valid\)/s_\1_\2_\3/g' $@
	@sed -i -e '/^  .*DummyDPICWrapper/i\`ifndef SYNTHESIS' \
	-e '/^  .*DummyDPICWrapper/{:a N; /;/!ba s/;/;\n`endif/ };' $@
	@sed -i -e '/^  .*Delayer/i\`ifndef SYNTHESIS' \
	-e '/^  .*Delayer/{:a N; /;/!ba s/;/;\n`endif/ };' $@

verilog: $(TOP_V)

$(SIM_TOP_V): $(SCALA_FILE) $(TEST_FILE)
	mkdir -p $(@D)
	@echo "\n[mill] Generating Verilog files..." > $(@D)/time.log
	@date -R | tee -a $(@D)/time.log
	time -o $(@D)/time.log mill -i XiangShan.test.runMain $(SIMTOP) -td $(@D) \
		--config $(CONFIG) --full-stacktrace --num-cores $(NUM_CORES) \
		$(SIM_ARGS) --target systemverilog | tee build/make.log
ifeq ($(VCS), 1)
	@sed -i $$'s/$$fatal/assert(1\'b0)/g' $@
else
	@sed -i -e 's/$$fatal/xs_assert(`__LINE__)/g' $@
endif
	@python3 scripts/assertion_alter.py -o $@ $@
	@sed -i "s/assign \([a-zA-Z0-9_]\+\) = .* ? \(.*\) : [0-9]\+'bx;/assign \1 = \2;/g" $@
	@sed -i 's/_LOG_MODULE_PATH_/%m/g' $@
	@sed -i 's/\(\b[a-zA-Z_0-9]\+_[0-9]\+x[0-9]\+\b\)/$(PREFIX)\1/g' $@
	@sed -i '/\/\/ ----- 8< ----- FILE "firrtl_black_box_resource_files.f" ----- 8< -----/,$$d' $@
	@sed -i -e 's/\(peripheral\|memory\)_0_\(aw\|ar\|w\|r\|b\)_bits_/m_\1_\2_/g' \
	-e 's/\(dma\)_0_\(aw\|ar\|w\|r\|b\)_bits_/s_\1_\2_/g' $@
	@sed -i -e 's/\(peripheral\|memory\)_0_\(aw\|ar\|w\|r\|b\)_/m_\1_\2_/g' \
	-e 's/\(dma\)_0_\(aw\|ar\|w\|r\|b\)_\(ready\|valid\)/s_\1_\2_\3/g' $@
	@sed -i -e '/^  .*DummyDPICWrapper/i\`ifndef SYNTHESIS' \
	-e '/^  .*DummyDPICWrapper/{:a N; /;/!ba s/;/;\n`endif/ };' $@
	@sed -i -e '/^  .*Delayer/i\`ifndef SYNTHESIS' \
	-e '/^  .*Delayer/{:a N; /;/!ba s/;/;\n`endif/ };' $@

FILELIST := $(ABS_WORK_DIR)/build/cpu_flist.f

sim-verilog: $(SIM_TOP_V)
	find $(ABS_WORK_DIR)/build -name "*.v" > $(FILELIST)
	find $(ABS_WORK_DIR)/build -name "*.sv" >> $(FILELIST)

clean:
	$(MAKE) -C ./difftest clean
	rm -rf ./build
	rm -rf ./sim

init:
	git submodule update --init
	cd coupledL2 && git submodule update --init AXItoTL
	cd rocket-chip && git submodule update --init cde hardfloat

bump:
	git submodule foreach "git fetch origin&&git checkout master&&git reset --hard origin/master"

comp:
	mill -i XiangShan.compile
	mill -i XiangShan.test.compile

bsp:
	mill -i mill.bsp.BSP/install

idea:
	mill -i mill.idea.GenIdea/idea

# verilator simulation
emu:
	$(MAKE) -C ./difftest emu SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) WITH_DRAMSIM3=$(WITH_DRAMSIM3)

emu_rtl:
	$(MAKE) -C ./difftest emu SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) WITH_DRAMSIM3=$(WITH_DRAMSIM3) SIMDIR=1 EMU_TRACE=1 EMU_THREADS=16

RANDOM = $(shell echo $$RANDOM)
EMU_RUN_OPTS = -i $(RUN_BIN_DIR)/$(RUN_BIN)
EMU_RUN_OPTS += --diff $(ABS_WORK_DIR)/ready-to-run/riscv64-nemu-interpreter-so
EMU_RUN_OPTS += --wave-path $(ABS_WORK_DIR)/sim/emu/$(RUN_BIN)/tb_top.vcd
EMU_RUN_OPTS += --enable-fork --fork-interval=15 -s $(RANDOM)
emu_rtl-run:
	$(shell if [ ! -e $(ABS_WORK_DIR)/sim/emu/$(RUN_BIN) ];then mkdir -p $(ABS_WORK_DIR)/sim/emu/$(RUN_BIN); fi)
	touch sim/emu/$(RUN_BIN)/sim.log
	$(shell if [ -e $(ABS_WORK_DIR)/sim/emu/$(RUN_BIN)/emu ];then rm -f $(ABS_WORK_DIR)/sim/emu/$(RUN_BIN)/emu; fi)
	ln -s $(ABS_WORK_DIR)/sim/emu/comp/emu $(ABS_WORK_DIR)/sim/emu/$(RUN_BIN)/emu
	cd sim/emu/$(RUN_BIN) && (./emu $(EMU_RUN_OPTS) 2> assert.log | tee sim.log)

# vcs simulation
simv:
	$(MAKE) -C ./difftest simv SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) CONSIDER_FSDB=$(CONSIDER_FSDB) VCS=1 REF=Spike

simv-run:
	$(shell if [ ! -e $(ABS_WORK_DIR)/sim/rtl/$(RUN_BIN) ];then mkdir -p $(ABS_WORK_DIR)/sim/rtl/$(RUN_BIN); fi)
	touch sim/rtl/$(RUN_BIN)/sim.log
	$(shell if [ -e $(ABS_WORK_DIR)/sim/rtl/$(RUN_BIN)/simv ];then rm -f $(ABS_WORK_DIR)/sim/rtl/$(RUN_BIN)/simv; fi)
	$(shell if [ -e $(ABS_WORK_DIR)/sim/rtl/$(RUN_BIN)/simv.daidir ];then rm -rf $(ABS_WORK_DIR)/sim/rtl/$(RUN_BIN)/simv.daidir; fi)
	ln -s $(ABS_WORK_DIR)/sim/rtl/comp/simv $(ABS_WORK_DIR)/sim/rtl/$(RUN_BIN)/simv
	ln -s $(ABS_WORK_DIR)/sim/rtl/comp/simv.daidir $(ABS_WORK_DIR)/sim/rtl/$(RUN_BIN)/simv.daidir
	cd sim/rtl/$(RUN_BIN) && (./simv $(RUN_OPTS) 2> assert.log | tee sim.log)

verdi:
	cd sim/rtl/$(RUN_BIN) && verdi -sv -2001 +verilog2001ext+v +systemverilogext+v -ssf tb_top.vf -dbdir simv.daidir -f sim_flist.f

.PHONY: verilog sim-verilog emu clean help init bump bsp $(REF_SO)

