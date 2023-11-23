-ifndef(updater_hw_devices_defines).
-define(updater_hw_devices_defines, true).

-define(MODULE_NAME, "updater_hw_devices").

-define(PLAT1, "etsc1").
-define(PLAT2, "etsc2").
-define(PLAT6, "etsc6").
-define(TEST, "test").

-define(NULL, "").
-define(LC4, "lc4").
-define(SC2000, "sc2000").
-define(MNGT, "mngt").
-define(LC1, "lc1").
-define(LC5, "lc5").

-define(NOT_IMPLEMENTED, "not implemented").

% ToDo: replace the IMAGES_PATH to the correct path
%-define(IMAGES_PATH, "/opt/ets/hw/images/").
-define(IMAGES_PATH, "/home/adilson/opt/ets/hw/images/").
-define(SPK_PARTITION, "/mnt/update").
-define(KERNEL_CMDLINE, "/proc/cmdline").
-define(KERNEL_BZIMAGE, "/boot/bzImage").
-define(KEXEC_TIMEOUT, "2").
-define(DEVICES_VERSIONS_FILE, "/data/board/hw_devices_versions.csv").

-define(GPIOGET, "/usr/bin/gpioget").
-define(GPIOSET, "/usr/bin/gpioset").
-define(GPIOFIND, "/usr/bin/gpiofind").
-define(FPGAIO, "/usr/bin/fpgaio").
-define(I2CGET, "/usr/sbin/i2cget").
-define(I2CSET, "/usr/sbin/i2cset").
-define(I2CTRANSFER, "/usr/sbin/i2ctransfer").
-define(I2CDETECT, "/usr/sbin/i2cdetect").
-define(FANSCRIPT, "/usr/bin/fan").
-define(DD, "/bin/dd").
-define(BTOOL, "/usr/bin/btool").
-define(MD5SUM, "/usr/bin/md5sum").
-define(MX25U256, "/dev/mx25u256").
-define(MACHXO2, "/usr/bin/machxo2").
-define(BLHOST, "/usr/bin/blhost").
-define(KEXEC, "/usr/sbin/kexec").
-define(ISSI_FLASH, "/usr/bin/issi_flash").
-define(SPI_CPLD_DRIVER, "/dev/spidev50.2").
-define(DATA_BOARD_PATH, "/data/board/").
-define(FLASHROM, "/usr/sbin/flashrom").
-define(EHALCLI, "/usr/bin/ehalcli").

-define(NOHUP, "/usr/bin/nohup").
-define(NOTIFY_SEND, "/usr/bin/notify-send").
-define(NPROC, "/usr/bin/nproc").
-define(NROFF, "/usr/bin/nroff").
-define(NSENTER, "/usr/bin/nsenter").
-define(NSLOOKUP, "/usr/bin/nslookup").
-define(NSS_ADDBUILTIN, "/usr/bin/nss-addbuiltin").
-define(NSS_DBTEST, "/usr/bin/nss-dbtest").
-define(NSS_PP, "/usr/bin/nss-pp").
-define(NSTAT, "/usr/bin/nstat").
-define(NSUPDATE, "/usr/bin/nsupdate").
-define(NTFS_3G, "/usr/bin/ntfs-3g").
-define(NTFS_3G_PROBE, "/usr/bin/ntfs-3g.probe").
-define(NTFSCAT, "/usr/bin/ntfscat").
-define(NTFSCLUSTER, "/usr/bin/ntfscluster").
-define(NTFSCMP, "/usr/bin/ntfscmp").
-define(NTFSDECRYPT, "/usr/bin/ntfsdecrypt").
-define(NTFSFALLOCATE, "/usr/bin/ntfsfallocate").
-define(NTFSFIX, "/usr/bin/ntfsfix").
-define(NTFSINFO, "/usr/bin/ntfsinfo").
-define(NTFSLS, "/usr/bin/ntfsls").
-define(NTFSMOVE, "/usr/bin/ntfsmove").
-define(NTFSRECOVER, "/usr/bin/ntfsrecover").
-define(NTFSSECAUDIT, "/usr/bin/ntfssecaudit").
-define(NTFSTRUNCATE, "/usr/bin/ntfstruncate").
-define(NTFSUSERMAP, "/usr/bin/ntfsusermap").
-define(NTFSWIPE, "/usr/bin/ntfswipe").
-define(NUMFMT, "/usr/bin/numfmt").
-define(NVIDIA_DETECTOR, "/usr/bin/nvidia-detector").
-define(NVLC, "/usr/bin/nvlc").

-endif.
