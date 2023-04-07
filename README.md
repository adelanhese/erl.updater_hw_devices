
  1 - This app scans all devices on the board, checking their versions.
      If an outdated device is found, it can automatically update itself.

      This app uses a configuration file that contains all the information
      needed to verify and update all the devices on the board.

      The name of this configuration file must follow the following format:

               <platform>_devices.cfg

      This file must be located in the hw images folder:

               /opt/ets/hw/images/<platform>

  2 - Example in the case of MYPLAT1:

              /opt/ets/hw/images/myplat1
                       |
                       └── myplat1_devices.cfg


  3 - Configuration file structure

       The configuration file must contain two type of sections:

             [boards] and [<board_name>].

       3.1 - The [board] section must contain the follow parameters:

             num_borard  => defines the number of boards to be scanned
             board(n)    => defines the boards names

                Note: (n) is the index of the board [1 ... n]

       3.2 - The [<board_name>] section must contain the follow parameters:

             num_devices   => defines the number of devices on the board
             device(n)     => defines the device type (eg. fpga, cpld, etc)
             alias         => used to differentiate the same devices with
                              possible different configurations (ex: otu2 or otu4)
             model(n)      => defines the device model (eg. stratix, machx02, etc)
             version(n)    => defines the version of device
             file(n)       => defines the full path and file name of configuration image
             md5(n)        => defines the md5 sum string of file configuration image
             devport(n)    => entry point to device port (ex: /dev/i2c-5)
             adapter(n)    => adapter number to access the device
             activecard(n) => defines if the update needs to run on the active card only
             enabled(n)    => enable/disable the device during check/update scan
             checkversion(n)=> enable/disable the device during check version scan
             dependencies(n)=> name of dependent device

                Note: 1) (n) is the index of the device inside of the board [1 ... n]
                      2) To calculate the md5 string, just run:
                             md5sum <file configuration image name>
                         and after that, copy and past the result output

       3.3 - Example:

             [boards]
             num_boards = 2
             board1 = lc1
             board2 = fan
             
             [lc1]
             num_devices = 5
             device1 = fpgajic
             alias1 = otu2
             model1 = stratix
             version1 = 3GW00356AAAA24
             file1 = /opt/ets/hw/images/myplat1/lc1/fpga/10g/3GW00356AAAA24_FPGA_LC1_TOP_REV1_0_jic.rpd
             md51 = 84bfec9a08ed142db6e9a5fa4b07015a
             devport1 = /dev/mx25u256
             adapter1 = 0
             activecard1 = 0
             enabled1 = 0
             checkversion1 = 1
             dependencies1 = lc1_fpgacvp/otu2
             
             device2 = fpgacvp
             model2 = stratix
             alias2 = otu2
             version2 = 3GW00356AAAA24
             file2 = /opt/ets/hw/images/myplat1/lc1/fpga/10g/3GW00356AAAA24_FPGA_LC1_TOP_REV1_0.core.rbf
             md52 = ccdc5d423beb17278ca4a6b406af14e2
             devport2 = /dev/altera_cvp
             adapter2 = 0
             activecard2 = 0
             enabled2 = 0
             checkversion2 = 0
             dependencies2 = lc1_fpgajic/otu2
             
             device3 = fpgajic
             alias3 = otu4
             model3 = stratix
             version3 = 3GW00339ABAA69
             file3 = /opt/ets/hw/images/myplat1/lc1/fpga/100g/3GW00339ABAA69_01_lc1_fpga_jic.rpd
             md53 = 82ca92f367f82fa7085e610cd7caa268
             devport3 = /dev/mx25u256
             adapter3 = 0
             activecard3 = 0
             enabled3 = 1
             checkversion3 = 1
             dependencies3 = lc1_fpgacvp/otu4
             
             device4 = fpgacvp
             model4 = stratix
             alias4 = otu4
             version4 = 3GW00339ABAA69
             file4 = /opt/ets/hw/images/myplat1/lc1/fpga/100g/3GW00339ABAA69_01_lc1_fpga.core.rbf
             md54 = 0eab19a4d7a7356fe04e6c84536ec81e
             devport4 = /dev/altera_cvp
             adapter4 = 0
             activecard4 = 0
             enabled4 = 1
             checkversion4 = 0
             dependencies4 = lc1_fpgajic/otu4
             
             device5 = lpc55
             model5 = s66jev98
             alias5 =
             version5 = 1.0.202203160734
             file5 = /opt/ets/hw/images/myplat1/lc1/lpc55s6x/lpc55s66jev98_myplat1_lc_1_0_202203160734.hex
             md55 = df4914b3c06c84a62acf6b1a0a79a56b
             devport5 = /dev/i2c-5
             adapter5 = 5
             activecard5 = 0
             enabled5 = 1
             checkversion5 = 1
             dependencies5 =
             
             [fan]
             num_devices = 2
             device1 = cpld
             model1 = machx02
             alias1 = 
             version1 = 4
             file1 = /opt/ets/hw/images/myplat1/fan/cpld/CPLD_FAN_MYPLAT1_v04.jed
             md51 = ac4b38fc63716157bfdd6f14680df55d
             devport1 = /dev/i2c-34
             adpater1 = 34
             activecard1 = 1
             enabled1 = 1
             checkversion1 = 1
             dependencies1 =
             
             device2 = lpc55
             model2 = s69
             alias2 = 
             version2 = 1.0.202203210757
             file2 = /opt/ets/hw/images/myplat1/fan/lpc55s6x/lpc55s69_myplat1_fan_1_0_202203210757.hex
             md52 = aa757761ad56e872e6e3a6e097c6118d
             devport2 = /dev/i2c-34
             adapter2 = 34
             activecard2 = 1
             enabled2 = 1
             checkversion2 = 1
             dependencies2 =

  4 - How to add new procedure to check and update a device:

      4.1 Fill the configration file

          First of all, you need to fill the configuration file with the
          information need to check and update the new device (see the example above)


       4.2 Create the four new procedures to check and update the device

           4.2.1 Proceure to check the version

                  You need to create one procedure to check the version of the new device. 
                  The name of this new procedure must follow the rule:

                       get_version_<platform_name>_<board_name>_<device_name>

           4.2.2 Procedure to update the device

                  You need to create one procedure to update the new device. 
                  The name of this new procedure must follow the rule:

                       update_<platform_name>_<board_name>_<device_name>

           4.2.3 Procedure to perform the power cycle in the board

                  You need to create one procedure to perform the power cycle on the board. 
                  The name of this new procedure must follow the rule:

                       power_cycle_<platform_name>_<board_name>

           4.2.4 Procedure to get the slot ID in the board

                  You need to create one procedure to get the slot ID on the board. 
                  The name of this new procedure must follow the rule:

                       get_slot_id_<platform_name>_<board_name>


   Note: 1-No changes in the main code will be necessary to add new devices in this app. Only these 4 new procdures will be necessary.
         2-Each platform must have your own module. Example: "updater_hw_devices_<platform_name>.erl"
         2-These procedures describled in the item 4, will be add in the correct module: 


