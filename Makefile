all:
	$(MAKE) --directory dos65_os
	$(MAKE) --directory 6x0x_firmware
	$(MAKE) --directory 6502PC_firmware
	$(MAKE) --directory nhyodyne_firmware
	$(MAKE) --directory duodyne_firmware
	$(MAKE) --directory duodyne02_firmware
	$(MAKE) --directory dos65_utilities
	$(MAKE) --directory software/ZMachine
	$(MAKE) --directory software/dbasic

pretty:
	$(MAKE) --directory 6x0x_firmware pretty
	$(MAKE) --directory 6502PC_firmware pretty
	$(MAKE) --directory nhyodyne_firmware pretty
	$(MAKE) --directory dos65_os pretty
	$(MAKE) --directory dos65_utilities pretty
	$(MAKE) --directory duodyne_firmware pretty
	$(MAKE) --directory duodyne02_firmware pretty
	$(MAKE) --directory software/ZMachine pretty
	$(MAKE) --directory software/dbasic pretty

clean:
	$(MAKE) --directory 6x0x_firmware clean
	$(MAKE) --directory 6502PC_firmware clean
	$(MAKE) --directory nhyodyne_firmware clean
	$(MAKE) --directory dos65_os clean
	$(MAKE) --directory dos65_utilities clean
	$(MAKE) --directory duodyne_firmware clean
	$(MAKE) --directory duodyne02_firmware clean
	$(MAKE) --directory software/ZMachine clean
	$(MAKE) --directory software/dbasic clean