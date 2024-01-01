bios_driveParamSize:
	DW 0      ! 0
bios_driveFlags:
	DW 0      ! 2
bios_numberPhyCylinders:
	DW 0      ! 4
	DW 0      ! 6
bios_numberPhyHeads:
	DW 0      ! 8
	DW 0      ! 10
bios_numberPhySectorsPerTrack:
	DW 0      ! 12
	DW 0      ! 14
bios_numberTotalSectorsOnDrive:
	DW 0      ! 16
	DW 0      ! 18
	DW 0      ! 20
	DW 0      ! 22
bios_bytesPerSector:
	DW 0      ! 24
remainder:
	DW 0      ! 26
	DW 0      ! 28
	DW 0      ! 30
	DW 0      ! 32




	! ---- cut here ----

	lds     si, bios_driveParamSize
	mov     ah, #0x48    ! get geometry via bios
	int     #0x13
	jc      getgeoerror
	pop     ax
	mov     ds, ax       ! restore old ds
	mov     bx, bios_numberPhyHeads
	mov     cx, bios_numberPhySectorsPerTrack
	jmp     getgeoret
	
getgeoerror:
	mov     bx, ax       ! error code back in heads
	xor     cx, cx       ! and cylinders = 0
	pop     ax
	mov     ds, ax       ! restore old ds
