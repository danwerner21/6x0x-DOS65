all:
	$(MAKE) --directory 6x0x_firmware
	$(MAKE) --directory dos65_os
	$(MAKE) --directory nhyodyne_firmware
	$(MAKE) --directory dos65_utilities

pretty:
	$(MAKE) --directory 6x0x_firmware pretty
	$(MAKE) --directory nhyodyne_firmware pretty
	$(MAKE) --directory dos65_os pretty
	$(MAKE) --directory dos65_utilities pretty

clean:
	$(MAKE) --directory 6x0x_firmware clean
	$(MAKE) --directory nhyodyne_firmware clean
	$(MAKE) --directory dos65_os clean
	$(MAKE) --directory dos65_utilities clean