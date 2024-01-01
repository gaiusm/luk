#if !defined(DeviceConfigurationH)
#   define DeviceConfigurationH

/*
   Title      : DeviceConfiguration
   Author     : Gaius Mulley
   Date       : 28/2/88
   LastEdit   : 7/9/94
   System     : RTS gm2
   Description: Provides a global module for device characteristics
                and device names.
*/


typedef enum {
  DeviceConfiguration_None, DeviceConfiguration_Odd, DeviceConfiguration_Even
} DeviceConfiguration_Parity;


/*
   GetIrqNo - returns the irq number for a UART.
              This function returns 256 if UART has no IRQ
              else IRQ level (2-7)
*/


extern unsigned int DeviceConfiguration_GetIrqNo (unsigned int BaseAddress);


#endif

