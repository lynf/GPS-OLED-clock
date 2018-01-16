/*
 * gps-oled-clock.asm
 *
 *  Created: 7/16/2016 12:31:18 PM
 *   Author : lynf
 */
;
;
;######################################################################################
; This software is Copyright by Francis Lyn and is issued under the following license:
;
; Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License
;
;######################################################################################
;
;
;
; **** THIS VERSION FOR USE WITH 0.96" or 1.3" OLED DISPLAY PANEL ****
;		and Verdana 24 pixel high variable width font set
;
; The OLED driver supports both SSD1306 and SH1106 graphic controller chips
; using SPI interface. Some 1.3" modules have a CS* pin brought out, others
; don't. CS* is grounded when it is provided.
;
; The GPS module interfaces to the controller via serial USART connection.
; The USART runs at 9600 baud and speed should not be changed because GPS
; data decoding takes place on-the-fly. You don't want the incoming data
; arriving faster than the micro-controler can handle.
;
; The gps clock program can run on a variety of controller boards such as
; the Arduino UNO, Nano or Pro-mini, and even on a single ATmega328P chip.
;
; For single chip ATmega328P device and Pro-mini (3.3 V version), the Fosc
; clock operates at 8.000 MHz. Set the following equates as shown below:
;
; .equ	Vcc_low = true
; .equ	baud_low = true
;
; For the UNO and Nano the Fosc clock operates at 16.000 MHz.
; Set the following equates as shown below:
;
; .equ	Vcc_low = false
; .equ	baud_low = true
;
;
; For a single chip ATmega328P running the gps clock program, the internal
; R/C oscillator operating at 8.000 MHz is used. You must program the low
; fuse byte to set CKSEL[3..0] fuse bits to [0019} and CKDIV = 1 before you
; run the clock. You set these bits in one step by programming LFUSE = 0xF2.
;
; The default time zone is -5 h, corresponding to Eastern Time Zone, Canada.
; The default TZoff is hard coded into the program by setting the equate
; TZoff as follows:
;
; .equ	TZoff = 5
;
; to the required value. Change this equate if you want to operate the clock in
; a different time zone.
;

.list		; Listing on

;
; General equates
;
.equ	FALSE = 0x0			; Logical 0
.equ	TRUE = !FALSE		; Logical 1
;.equ	Vcc_low = false		; False for 5 V controller board
.equ	Vcc_low = true		; True for 3.3 V controller board
.equ	debug = true		; Turn on debugging routines
;.equ	debug = false		; Turn on debugging routines
.equ	baud_low = true		; 9600 baud for later boards
;.equ	baud_low = false	; 19200 baud for 1st board
;
; GPS equates
;
.equ	LED = PD4			; D4, test LED
.equ	time_sz = 9			; Time string length
.equ	date_sz = 6			; Date string length
.equ	fieldskip = 8		; Fields to skip
.equ	TZoff = 5			; TZ offset, default value for Ontario, Canada
;.equ	TZoff = 8			; TZ offset, default value for Beijing, China
.equ	TZsign = 0			; Set = 1 for positive TZ offset
.equ	DST_en = PC0		; Enable DST correction
;
; Processor operating voltage
;

.if	Vcc_low
.equ F_CPU = 8000000
.else
.equ F_CPU = 16000000
.endif

;
; $GPRMC header data
;
.equ	hdr_sz = 6
;
;
; UART definitions
;
;
.if		baud_low
.if		Vcc_low
.equ	BAUD_PRE = 51		; Baud rate prescaler - 8.00 MHz clock, 9600
.else
.equ	BAUD_PRE = 103		; Baud rate prescaler - 16.00 MHz clock, 9600
.endif
;
.else
;
.if		Vcc_low
.equ	BAUD_PRE = 25		; Baud rate prescaler - 8.00 MHz clock
.else
.equ	BAUD_PRE = 51		; Baud rate prescaler - 16.00 MHz clock
.endif
;
.endif
;
.equ	NULL = 0x0			; Null terminator
.equ	BELL = 0x07			; Bell
.equ	BS = 0x08			; Backspace
.equ	HT = 0x09			; Tab
.equ	LF = 0x0a			; Linefeed
.equ	CR = 0x0d			; Carriage return
.equ	ctlW = 0x17			; Control W
.equ	ctlX = 0x18			; Control X
.equ	ctlZ = 0x1a			; Control Z
.equ	SP = 0x20			; Space
.equ	ESC = 0x1b			; Escape
.equ	DEL = 0x7f			; Delete
.equ	CMA	= 0x2c			; Comma
.equ	at = 0x40			; '@'
;


.if		Vcc_low

; Timer0, Timer1 and Timer2 parameters for 8.00 MHz clock
;
; Prescaler:	1		8		64			256			1024
; TCNTn clk:	8 MHz	1 MHz	125 kHz		31.25 kHz	7.8125 kHz
; Period:				1 us	8 us		32 us		128 us
;
; TCNT0 prescaler = 1024, clk_T0 = 8 MHz/1024 = 7.8125 kHz, 128 us
; TCNT1 prescaler = 256, clk_T1 = 8 MHz/1024 = 7.8125 kHz, 128 us
; TCNT2 prescaler = 1024, clk_T2 = 8 MHz/1024 = 7.8125 kHz, 128 us
;
;
.equ	OCR0Aload = 32		; OCR0A 8 bit register, 32 x 128 us = 4.096 ms
;

.else

;
; Timer0, Timer1 and Timer2 parameters for 16.00 MHz clock
;
; Prescaler:	1		8		64			256			1024
; TCNTn clk:	16 MHz	2 MHz	250 kHz		62.5 kHz	15.625 kHz
; Period:				0.5 us	4 us		16 us		64 us
;
; TCNT0 prescaler = 1024, clk_T0 = 16 MHz/1024 = 15.625 kHz, 64 us
; TCNT1 prescaler = 256, clk_T1 = 16 MHz/1024 = 15.625 kHz, 64 us
; TCNT2 prescaler = 1024, clk_T2 = 16 MHz/1024 = 15.625 kHz, 64 us
;
;
.equ	OCR0Aload = 64		; OCR0A 8 bit register, 64 x 64 us = 4.096 ms

.endif
;
;
;
;###########################################################################
;
; Font description definitions for selected oled font
; Font table from openGLCD font tables. A variable width font is
; used. Font height is in byte multiples, 1 page per byte. The 0.96
; oled display has 8 pages, so each character fits on number of pages
; determined by height of fonts used. Verdana 24 fonts are 24 pixels
; high, all fonts have same height.
;
; The font table has all the required font descriptors
;
; =========================================================================
;
; Variable width fonts have font width information in the font table.
; Routines for searching the font table need to compute font pointer
; address by adding individual font widths to get the correct offset
; to specified character address in font table.
;
;
; Verdana_digits_24
;
.equ	max_font_width = 17+3	; 'space' pixel width + padding
.equ	font_height_page = 3	; Character height in pages
;
;
;###########################################################################
;
; OLED 0.96 inch 128 x 64 module definitions
;
; oled control pins for 4 wire SPI interface
;
.equ	ol_cs = PB2			; D10 control pin, CS* - Not on some modules
;
.equ	ol_RST = PB0		; D8 control pin, Reset
.equ	ol_A0 = PB1			; D9 (A0) control pin, Data/command
;
.equ	MOSI = PB3			; D11 Master output to slave
.equ	SCK = PB5			; D13 Master clock output
;
; SSD1306 basic commands
;
.equ	ol_set_lo_col = 0X00			; Set lower col start address, page addressing mode mode (0x00 ~ 0x0f)
.equ	ol_set_hi_col = 0X10			; Set higher col start address, page addressing mode mode (0x10 ~ 0x1f)
.equ	ol_set_mem_addr = 0x20			; Set memory addressing mode, 2 byte command
										; A[1,0] = 10 page addressing mode
										; A[1,0] = 00 horizontal addressing mode, raster scanning
										; A[1,0] = 01 vertical addressing mode
.equ	ol_set_col_addr = 0x21			; Set column address, 3 byte command
.equ	ol_set_page_addr = 0x22			; Set page address, 3 byte command
.equ	ol_set_start_line = 0x40		; Set display start line (0x40 ~ 0x7f)
.equ	ol_set_contrast = 0x81			; Set display contrast, bank0, 2 byte command
.equ	ol_set_segment_remap0 = 0xa0	; Set segment remap 0xa0
.equ	ol_set_segment_remap1 = 0xa1	; Set segment remap 0xa1
.equ	ol_set_NOP = 0xff				; No operation
.equ	ol_set_mux_ratio = 0xa8			; Set multiplex ratio
.equ	ol_set_page_start_addr = 0xb0	; Set page start address (0xb0 ~ 0xb7)
.equ	ol_set_com_scan_dir = 0xc8		; Set COM scan direction (0xc0 ~ 0xc8)
.equ	ol_set_display_offset = 0xd3	; Set display offset, 2 byte command
.equ	ol_set_clock_divider = 0xd5		; Set clock divide ratio
										; A[3:0] = clock divide ratio
										; A[7:4] = oscillator frequency
.equ	ol_set_enable_charge_pmp = 0x8d	; Enable charge pump regulator
.equ	ol_set_precharge = 0xd9			; Set precharge period
.equ	ol_set_Vcomh = 0xdb				; Set Vcomh regulator output
.equ	ol_set_com_config = 0xda		; Set COM pins configuration
.equ	ol_set_whole_display_on = 0xa4	; Set entire display on (0xa4 ~ 0xa5)
.equ	ol_set_normal = 0xa6			; Set normal diaplay 0xa6
.equ	ol_set_reverse = 0xa7			; Set reverse diaplay 0xa7
.equ	ol_set_no_scroll = 0x2e			; Set no-scrolling
.equ	ol_set_display_on = 0xaf		; Set display ON 0xaf
.equ	ol_set_display_off = 0xae		; Set display OFF, sleep mode 0xae
;
; SSD1306 graphical acceleration commands
;
;###########################################################################
;
;
;
; Flag register flaga
;
.equ	numfl = 0			; Valid byte number flaga
.equ	crf = 1				; Carriage return key flaga
.equ	escf = 2			; Escape key flaga
.equ	kyf	= 3				; Control key flaga
.equ	xclinf = 4			; Delayed line clear flaga
.equ	tcnt1fa = 5			; TCNT1 flaga software timer flag
.equ	t4msf = 6			; 4.096 ms timer tick flag
.equ	TZposf = 7			; TZ +vs or -ve sign flag
;
; Flag register flagb
;
; Line buffer size
;
.equ	linesz = 128*font_height_page		; Line buffer size
;
;
;
; --- Register definitions ---
;
; Low registers
;
.def	count = R2			; Counter for line buffer
.def	asav = R3			; rga save register
.def	SRsav = R4			; SREG save
;
;
; High registers
;
.def	rmp = R16			; Multipurpose register
.def	rga = R17			; GP register RGA
.def	rgb = R18			; GP register RGB
.def	rgc = R19			; GP register RGC
.def	rgd = R20			; GP register RGD
.def	rge	= R21			; GP register RGE
.def	rgv	= R22			; Variable register
.def	flaga = R23			; Flag A register, 8 flags
.def	flagb = R24			; Flag B register, 8 flags
;
;
;
; --- Macro definitions ---
;
.macro	ldzptr				; Load ZH:ZL pointer with address*2
		ldi		ZH,high(@0*2)
		ldi		ZL,low(@0*2)
.endm
;
.macro	ldxptr				; Load XH:XL pointer with address to access data memory
		ldi		XH,high(@0)
		ldi		XL,low(@0)
.endm
;
.macro	ldyptr					; Load YH:YL pointer with address to access data memory
		ldi		YH,high(@0)
		ldi		YL,low(@0)
.endm
;
; Exchange contents of registers
;
.macro	xchreg					; Exchange registers
		push	@0
		push	@1
		pop		@0
		pop		@1
.endm
;
;
; --- SRAM Data Segment ---
;
.DSEG
.ORG	0X0100				; 2 Kb SRAM space
;
;
; RTC clock module data buffers
;
stbuf:						; Space for GPS data buffers
;
timeb:
.byte		9				; time buffer byte
;
dateb:
.byte		6				; Date
;
TZb:
.byte		1				; TZ offset value
;
LT_hourb:
.byte		2				; Local time hours
;
; Font data buffers
;
font_pageb:
.byte	1					; Character start page position on line
;
font_colb:
.byte	1					; Character start column position on line
;
font_widthb:
.byte	1					; Font width from font table
;
font_heightb:
.byte	1					; Font height from font table
;
font_first_charb:
.byte	1					; Font first character in font table
;
font_char_countb:
.byte	1					; Number of characters in font table
;
font_offsb:
.byte	1					; Offset bytes to character
;
line_csrH:					; Linebuf pointerH;
.byte	1
;
line_csrL:					; Linebuf pointerL
.byte	1
;
pageb:
.byte	1					; Page buffer for writing oled (SH1106 specific)
;
enbuf:
;
;
linebuf:
.byte	linesz				; Character input line buffer
;
;
; ============================================
;   R E S E T   A N D   I N T   V E C T O R S
; ============================================
;
;
; --- Code Segment ---
;
.CSEG
.ORG	$0000						; Interrupt vectors go here
;
		jmp			start			; Reset vector
;
;
.ORG	OC0Aaddr
		jmp			Timer0_COMPA	; Timer 0 Output Compare A handler
;
;
;
; End of interrupt vectors, start of program code space
;
;
.ORG	0x0034					; Program begins here
;
;
;###########################################################################
;
;
;
; ============================================
;     I N T E R R U P T   S E R V I C E S
; ============================================
;
;
; --- Timer 0 interrupt handler ---
;
; Used for I/O scanning
;
; TCNT0 run in Output Compare mode, using OCR0A register to
; generate output compare interrupt every 64 x 64 us = 4.096 ms.
;
; TCNT0 operates in Clear Timer on Compare Match (WGM02:0 = 2).
; On Compare Match, TCNT0 counter is cleared.
; OCR0A (set to 64) defines the counter's TOP value.
;
; Clk_T0 = 16 MHz/1024 = 15.625 kHz, 64 us period,
;
Timer0_COMPA:
;
		push	rmp					; Save registers
		in		SRsav,SREG
;
		sbr		flaga,(1<<t4msf)	; Set timer 0 overflow flag
;
		out		SREG,SRsav			; Restore SREG
		pop		rmp
		reti
;
;
;
;
;###########################################################################
;
;
; ============================================
;         Initialization routines
; ============================================
;
;
; Turn off watchdog
;
wdt_off:
		cli							; Clear global interrupts
;
; Reset WD timer
;
		wdr
;
		in		rmp,MCUSR				; Clear WDRF bit
		andi	rmp,(0xff & (0<<WDRF))	; WDRF bit = 0
		out		MCUSR,rmp
;
; Set WDCE and WDE bits, keep old prescaler setting
;
		lds		rmp,WDTCSR
		ori		rmp,(1<<WDCE)|(1<<WDE)
		sts		WDTCSR,rmp
;
; Turn off WDT
;
		ldi		rmp,(0<<WDE)			; Clear WD system reset enable
		sts		WDTCSR,rmp
;
		sei								; Set global interrupts
		ret
;
; --- Initialization Routines ---
;
initz:
		rcall	zbuf			; Clear data space buffers
;
		rcall	zregs			; Clear lower registers R0,..,R15
		clr		flaga			; Clear flag registers
		clr		flagb
;
;
; Activate pull-up resistors on all input pins, used and unused
;
		ldi		rmp,(1<<ol_cs)|(1<<ol_A0)|(1<<MOSI)|(1<<SCK)|(1<<ol_RST) ; Setup control outputs
		out		DDRB,rmp

		ldi		rmp,0b00011101
		out		PORTB,rmp
;
		ldi		rmp,0b00111111
		out		PORTC,rmp
;
;
; Select TZposf setting depending on value of TZsign equate
;
.if	TZsign
		sbr		flaga,(1<<TZposf)
.else
		cbr		flaga,(1<<TZposf)
.endif
;
;
; PD4 test LED driver output
;		sbi		DDRD,led			; Enable led output
;
; SPI module, enable SPI, master mode, fosc/64
;
		ldi		rmp,(1<<SPE)|(1<<MSTR)|(1<<SPR1)	; fosc/64
		out		SPCR,rmp
;
.if		Vcc_low

		ldi		rmp,(1<<SPI2X)		; Enable SPI double speed if 8 MHz clock
		out		SPSR,rmp			; on 3.3 V controller board

.endif
;
;
;
; --- Timers Initialization ----
;
;
; === TCNT0 Initialization ===
;
; Setup TCNT0 prescaler = 1024, clock period = 64 us
;
InitTimer0:
		ldi		rmp,(1<<CS02)|(1<<CS00)	; Divide by 1024 prescaler, Fclk = 15.625 kHz
		out		TCCR0B,rmp				; Timer/Counter0 control register B
;
; Setup TCNT0 for CTC mode
;
		ldi		rmp,(1<<WGM01)			; CTC mode
		out		TCCR0A,rmp				; Timer/Counter0 control register A
;
; Initialize OCR0A output compare register
;
		ldi		rmp,OCR0Aload			; Set OCR0A = 64 for 4.096 ms period
		out		OCR0A,rmp
;
; Enable Timer/Counter0 Compare A Match Interrput in TIMSK0
;
		lds		rmp,TIMSK0
		sbr		rmp,(1<<OCIE0A)			; Enable Timer/Counter0 Output Compare A Match Interrupt
		sts		TIMSK0,rmp
;
; --- Enable GLobal Interrupts
;
		sei							; Set global interrupts
		ret
;
; Initialize the UART for 9600 baud asynchronous operation
;
inzuart:
		cli							; Clear global interrupts
		ldi		rmp,high(BAUD_PRE)
		sts		UBRR0H,rmp			; Load baud rate register high
		ldi		rmp,low(BAUD_PRE)
		sts		UBRR0L,rmp			; Load baud rate register low
;
; Setup frame for 1 start, 8 data, 1 stop and no parity
;
		ldi		rmp,(1<<UCSZ00)|(1<<UCSZ01)
		sts		UCSR0C,rmp
;
; Enable the UART
;
		ldi		rmp,(1<<RXEN0)|(1<<TXEN0)
		sts		UCSR0B,rmp			; Enable RX and TX
		sei							; Set global interrupts
		ret
;
;
;
;###########################################################################
;
;     M A I N    P R O G R A M    S T A R T
;
;###########################################################################
;
;
;
; Controller startup and initialization
;
start:
;
; Initialize the stack pointer to end of SRAM
;
		ldi		rmp,high(RAMEND)	; Init MSB stack
		out		SPH,rmp
		ldi		rmp,low(RAMEND)		; Init LSB stack
		out		SPL,rmp
;
; Initialize the engine
;
		rcall	wdt_off			; Disable watchdog. Must be done soon after a reset
		rcall	initz			; Initialize engine
		rcall	inzuart			; Initialize the UART
;
; The oled display is a slow start-up device, add 100 ms delay after power-up
;
		rcall	oled_config				; Initialize the OLED display
		rcall	d_100m
		rcall	ol_clr_dram				; Clear ol dram
;
; Must call <set_font> before accessing font table
;
		rcall	set_font				; Define font parameters <<< Important >>>>
		rcall	line_1
	;	rcall	line_4
		rcall	clr_line

		rjmp	main

;
; =======================
;
	;	rjmp	print_string			; Print a test string, for testing
;
; Display one character from font table		(OK)
;
check_font:
		ldi		rga,'0'
		rcall	ol_wrdat

		ldi		rga,'1'
		rcall	ol_wrdat

		ldi		rga,'2'
		rcall	ol_wrdat

		ldi		rga,'3'
		rcall	ol_wrdat

		ldi		rga,'4'
		rcall	ol_wrdat

		rcall	wr_linebuf
		rcall	d500m
		rcall	clr_line

		ldi		rga,'4'
		rcall	ol_wrdat

		ldi		rga,'3'
		rcall	ol_wrdat

		ldi		rga,'2'
		rcall	ol_wrdat

		ldi		rga,'1'
		rcall	ol_wrdat

		ldi		rga,'0'
		rcall	ol_wrdat

		rcall	wr_linebuf
		rcall	d500m
		rcall	line_1
		rcall	clr_line

		rjmp	check_font

;
; 0.5 s delay
;
d500m:
		ldi		rmp,10
d500m1:
		rcall	d_100m
		dec		rmp
		brne	d500m1
		ret
;
; ==================================================================
;
; Print a string of numbers on oled		(OK)
;
print_string:
		ldzptr	testnums
		rcall	ol_pptr			; String to linebuf
		rcall	wr_linebuf		; Write linebuf to oled

stop_here:
		rjmp	stop_here
;
testnums:
.db		"0123456",ctlz

;
; ==================================================================
;
;
; Main program control loop
;
main:
		ldi		rmp,TZoff			; Use default TZ offset
		sts		TZb,rmp				; Load buffer <-- rmp
;
;  Dump GPS time and date data to screen and LCD module.
;
main0:
		rcall	scn_hdr			; Get GPS NMEA sentence header
		brcs	main0			; Wrong header, keep scanning
		ldxptr	timeb			; Store time string to timeb
		ldi		rgb,time_sz		; time string size
;
; Save time string from $GPSRMC sentence
;
main1:
		rcall	ci				; Get incoming time string
		st		X+,rga			; timeb <-- @X+
		dec		rgb
		brne	main1			; Continue saving time string
;
; Skip over to date string field
;
		ldi		rgb,fieldskip	; Fields to skip
main2:
		rcall	ci
		cpi		rga,','			; Comma field delimiter?
		brne	main2			; No, keep going
		dec		rgb
		brne	main2
;
; Save date string field
;
		ldxptr	dateb			; Store time string to timeb
		ldi		rgb,date_sz
main3:
		rcall	ci				; Get incoming date string
		st		X+,rga			; clkb <-- @X+
		dec		rgb
		brne	main3			; Continue saving date string
;
; GPS $GPRMC time and date date now saved in timeb and dateb buffers
;
		rcall	gmt_lt_hr		; Convert GMT to local time
;
		rcall	ol_prtime		; Display time string on LCD
	;	rcall	ol_prdate		; Display date string on LCD
;
		rjmp	main0
;
;
; Show time on oled line 1
;
ol_prtime:
		rcall	line_1			; Go to line 1
		rcall	clr_line
;
		lds		rga,LT_hourb	; MSD hours
		rcall	ol_wrdat		; Write to oled
		lds		rga,LT_hourb+1	; LSD hours
		rcall	ol_wrdat		; Write to oled
;
		ldi		rga,':'
		rcall	ol_wrdat		; Write to oled
;
		lds		rga,timeb+2		; MSD min
		rcall	ol_wrdat		; Write to oled
		lds		rga,timeb+3		; LSD min
		rcall	ol_wrdat		; Write to oled
;
		ldi		rga,':'
		rcall	ol_wrdat
;
		lds		rga,timeb+4		; MSD sec
		rcall	ol_wrdat		; Write to oled
		lds		rga,timeb+5		; LSD sec
		rcall	ol_wrdat		; Write to oled
;
		rcall	wr_linebuf
;
		ret
;
; Show date on oled line 4
;
ol_prdate:
		rcall	line_4			; Go to line 4
		rcall	clr_line
;
		lds		rga,dateb		; Day
		rcall	ol_wrdat		; Write to oled
		lds		rga,dateb+1
		rcall	ol_wrdat		; Write to oled
;
		ldi		rga,':'
		rcall	ol_wrdat		; Write to oled
;
		lds		rga,dateb+2		; Month
		rcall	ol_wrdat		; Write to oled
		lds		rga,dateb+3
		rcall	ol_wrdat		; Write to oled
;
		ldi		rga,':'
		rcall	ol_wrdat		; Write to oled
;
		lds		rga,dateb+4		; Year
		rcall	ol_wrdat		; Write to oled
		lds		rga,dateb+5
		rcall	ol_wrdat		; Write to oled
;
		rcall	wr_linebuf
;
		ret
;
;###########################################################################
;
;
; ==================================================================
;
; Clear line buffer
;
clr_line:
		ldxptr	linebuf			; Buffer pointer
		ldyptr	linesz			; Counter
		clr		rga				; Fill byte
clr_line0:
		st		X+,rga
		sbiw	YH:YL,1			; Count moves until done
		brne	clr_line0
		ret
;
; Write linebuf contents to oled dram at line_x position.
; SH1106 only does equivalent of SSD1306 page addressing mode,
; horizontal mode raster scanning is not supported. The page
; address must be changed explicitly after each page is
; written.
;
; Registers:
;	YH <-- col counter, 128 per page
;	YL <-- page counter
;
wr_linebuf:
		ldxptr	linebuf				; Buffer pointer
;
; Setup oled dram page address
;
		ldi		YL,font_height_page	; Buffer Pages
		lds		rmp,font_pageb		; Get oled page number
		sts		pageb,rmp			; Save to buffer
;
wr_lin0:
		ldi		YH,128				; Columns per page
		rcall	set_page_addr		; Write page address to oled
;
; Setup oled dram col address
;
		cbi		PORTB,ol_A0			; Command mode
		ldi		rga,ol_set_lo_col	; Col 0 lo
		inc		rga
		inc		rga					; Add 2 pixels
		rcall	ol_putb				; Send rga command to oled
		ldi		rga,ol_set_hi_col	; Col 0 hi
		rcall	ol_putb				; Send rga command to oled
		sbi		PORTB,ol_A0			; Data mode
wr_lin1:
		ld		rga,X+
		rcall	ol_putb
		dec		YH					; Count columns written
		brne	wr_lin1
		dec		YL
		brne	wr_lin0
		ret
;
; Set oled to page address passed in pageb, auto-increment page
;
set_page_addr:
		cbi		PORTB,ol_A0					; Command mode
		ldi		rga,ol_set_page_start_addr	; page start address (0xb0 ~ 0xb7) command
		lds		rmp,pageb					; Get current page
		add		rga,rmp						; Add selected page
		andi	rga,0xb7					; Mask to proper range
		inc		rmp							; Auto-increment next page
		sts		pageb,rmp					; Save new page
		rcall	ol_putb						; Send rga command to oled
		ret
;
;
;
; ==================================================================
;
;
; Writes font character @Z pointer to line buffer for later display
; on oled. Called by <wr_dat> which calls <get_font> to setup Z.	(OK)
;
; Registers:
;	ZHL pointer to font character in font table
;	YHL Pointer to linebuf target location
;	rga <-- font_colb, offset from linebuf start address
;	rgb <-- font_widthb
;	rgc	<-- font_height_page
;	rgd <-- general purpose
;	rmp <-- general purpose
;
put_char:
;
; YHL <-- linebuf target location on selected page
;
		lds		rga,font_colb			; Column offset from linebuf start address
		ldi		rgc,font_height_page	; OLED pages per character
;
; Setup pointer to linebuf start
;
		ldyptr	linebuf					; Setup linebuf pointer
;
; YHL points to linebuf start position. Add font_colb offset
; to Y for target address of this character in linebuf. Font_colb
; always has column start position of character to be written.
;
put_char0:
		add		YL,rga				; Add font_colb offset
		brcc	put_char1			; To YHL pointer
		inc		YH
;
; Y <-- target character's start address in linebuf, including
; font_colb offset. This address is saved in line_csrHL buffer
; for use in next page write for multi-page characters.
;
put_char1:
		sts		line_csrH,YH
		sts		line_csrL,YL		; Save for next page write
		lds		rgb,font_widthb		; Character column counter
;
put_char2:
		lpm		rmp,Z+				; Fetch font data from program memory
		st		Y+,rmp				; Write to linebuf
		dec		rgb					; Count columns for this page
		brne	put_char2
;
; Arrive here at end of a page write. Decrement page counter and write next
; page if not done.
;
		dec		rgc					; Check page counter
		brne	put_char3			; All pages written, finish up
		ret							; All done, exit
;
; More pages left, advance to next page, reset column counter
;
put_char3:
		lds		YH,line_csrH
		lds		YL,line_csrL		; Get page start position
;
		ldi		rgd,128				; Advance Y to next page
		add		YL,rgd
		brcc	put_char1
		inc		YH
		rjmp	put_char1
;
; Write rga to oled dram, increment cursor	(OK)
;
ol_wrdat:
		rcall	get_font		; Translate rga character to font Z pointer
		rcall	put_char		; Write font character @ZHL to linebuf
;
; Update font_colb for next character. Auto increment cursor
; to next column address past current character just written	(OK)
;
		lds		rmp,font_widthb		; Font width
		lds		rga,font_colb		; This character start col
		add		rga,rmp				; Advance to nest position
		inc		rga					; char spacing = 3 pixels
		inc		rga
		inc		rga
		sts		font_colb,rga		; Cursor at next position
;
		ret
;
;
;###########################################################################
;
; Set cursor to page and column start/end addresses
;
; Set fnt_page and fnt_col buffers to line0 start position
;
line_0:
		ldi		rmp,0				; Initialize buffers to line 0 start
		sts		font_pageb,rmp
		rjmp	line_ex
;
line_1:
		ldi		rmp,1				; Initialize buffers to line 1 start
		sts		font_pageb,rmp
		rjmp	line_ex
line_2:
		ldi		rmp,2				; Initialize buffers to line 2 start
		sts		font_pageb,rmp
		rjmp	line_ex
line_3:
		ldi		rmp,3				; Initialize buffers to line 3 start
		sts		font_pageb,rmp
		rjmp	line_ex
line_4:
		ldi		rmp,4				; Initialize buffers to line 4 start
		sts		font_pageb,rmp
		rjmp	line_ex
line_5:
		ldi		rmp,5				; Initialize buffers to line 5 start
		sts		font_pageb,rmp
		rjmp	line_ex
line_6:
		ldi		rmp,6				; Initialize buffers to line 6 start
		sts		font_pageb,rmp
		rjmp	line_ex
line_7:
		ldi		rmp,7				; Initialize buffers to line 7 start
		sts		font_pageb,rmp
;
line_ex:
		clr		rmp
		sts		font_colb,rmp
		rcall	set_csr				; Set cursor to current line
		ret
;
; Increment cursor position on current line to column address for
; next character on current line by pixels passed in rmp.
;
sp_px:
		lds		rga,font_colb			; Current cursor start column
		add		rga,rmp
		sts		font_colb,rga			; Update column buffer
		ret
;
; Set cursor to position defined by font_pageb/font_colb. The page and
; column start/end addresses are set up for writing a font character
; to dram.
;
set_csr:
		cbi		PORTB,ol_A0				; Command mode
;
; Set column start address based on font_colb current starting position
;
		ldi		rga,ol_set_lo_col	; Col 0 lo
		inc		rga
		inc		rga					; Add 2 pixels
		rcall	ol_putb				; Send rga command to oled
		ldi		rga,ol_set_hi_col	; Col 0 hi
		rcall	ol_putb				; Send rga command to oled
;
; Set page start address
;
		lds		rmp,font_pageb		; Get oled page number
		rcall	set_page_addr		; Write page address to oled
;
		sbi		PORTB,ol_A0				; Data mode
		ret
;
;
; ==================================================================
;
; Font table access routines for variable width fonts
;
; Set selected font parameters from font table to buffers in data RAM	(OK)
;
set_font:

		ldzptr	fontdata				; Load Z with (Font table address)*2
		lpm		rmp,Z+					; Read size, 2 bytes, not used
		lpm		rmp,Z+
;
		lpm		rmp,Z+					; Read font_width
		sts		font_widthb,rmp
;
		lpm		rmp,Z+					; Read font_height
		sts		font_heightb,rmp
;
		lpm		rmp,Z+					; Read font_first_char
		sts		font_first_charb,rmp
;
		lpm		rmp,Z+					; Read font_char_count
		sts		font_char_countb,rmp	; Character widths <-- Z
;
		ret
;
;
; Get font character representing ascii number in rga from font table	(OK)
;
; Entry:	rga <-- ASCII  number to print, from font_first_char to plus
;			font_char_cnt
;
; Exit:		Z pointer <-- target font address
;			rga destroyed
;			font_widthb <-- target font's width
;
get_font:
			lds		rmp,font_first_charb	; First font character ascii value
			sub		rga,rmp					; Convert to offset number
			sts		font_offsb,rga			; Save to font offset buffer
;
			brcs	get_font_ex			; Exit if ascii character < font_first_char
;
; Check if font offset is > font table size, (font_char_cnt)
;
get_font0:
			lds		rmp,font_char_countb	; Get font table number of characters
			dec		rmp					; Adjust for 0 based counting
			sub		rmp,rga
			brcc	get_font1			; Offset is within table size
get_font_ex:
			ret							; Beyond table, exit
;
; Character widths:
; Add up all char widths from start of table up to but not including
; target character. This total times char height (in pages of 8 bits)
; equals the number of bytes to advance Z pointer to the target font
; address.
;
; Compute font_width of selected character and save in font_widthb
;
get_font1:
;
; First test if 0 offset condition (rga=0), if true then at
; first character in table
;
			ldzptr	fontdata			; Load table pointer
			ldi		rmp,6				; Go past table header bytes
			rcall	Zrmp				; Advance to char widths
			lpm		rmp,Z				; Read char width and save in font_widthb
			sts		font_widthb,rmp		; First character's width
;
			tst		rga					; Offset 0?
			brne	get_font2			;	No
;
; Arrive here if first char in table
;
			ldzptr	fontdata
			ldi		rmp,6
			rcall	Zrmp
			lpm		rmp,Z
			sts		font_widthb,rmp
;
			ldzptr	fontdata
			lds		rga,font_char_countb	; Go past width bytes
			ldi		rmp,6				; Go past header bytes
			add		rmp,rga
			rcall	Zrmp				; Z at font data start address
			ret
;
; The font offset is non-zero, count number of columns to move to target character
;
; The font offset is non-zero, count number of columns to move to target character
; within font table. For large tables, column count may be larger than 255 so
; use 16 bit counter.
;
get_font2:
			clr		rgb					; Column counter		(OK)
			clr		XH
			clr		XL
get_font2a:
			lpm		rmp,Z+				; Pick up a width byte
			add		XL,rmp				; Total up columns to move in XHL
			adc		XH,rgb
			dec		rga					; Count offsets to character
			brne	get_font2a
;
get_font2b:
			lpm		rmp,Z				; Read char width and save in font_widthb;
			sts		font_widthb,rmp		; Target character's width
;
; Each font column has 'font_heightb' pixels.
; XHL <-- sum of columns to move to target font start address
; Compute Z to point to target font
;
; Advance Z pointer by XHL*font_heightb/8, (3 bytes per column)
;
			ldzptr	fontdata			; Load table pointer again
			lds		rga,font_char_countb	; Go past width bytes for 0 padding
			ldi		rmp,6				; Go past header bytes
			add		rmp,rga
			rcall	Zrmp				; Z at font data start address
;
; XHL <-- columns to move
; ZHL <-- Font data start address
; There are 3 bytes of data to move per column (3*8 = 24 pixels)
;
inc_zptr:
			ldi		rga,font_height_page
inc_zptr0:
			adiw	ZH:ZL,1				; Increment Z pointer
			dec		rga
			brne	inc_zptr0			; Font target character <-- Z
			sbiw	XH:XL,1				; Decrement columns to move
			brne	inc_zptr
;
; Arrive here with Z at target font data, font_widthb has target font width
;
			ret
;
; 16 bit add ZHL + rmp, return result in ZHL	(OK)
; Exit:	ZHL <-- rmp
;
Zrmp:
			push	XH
			push	XL
			clr		XH
			mov		XL,rmp
			add		ZL,XL
			adc		ZH,XH					; Z = Z + rmp, 16 bit addition
			pop		XL
			pop		XH
			ret

;
; Print ascii numstring on oled display. Z pointer is set to string address
; Test routine, not really useful for dynamic number displays
;
; Entry:	number string <-- Z pointer
;
ol_pptr:
		push	rga
ol_pptr1:
		lpm		rga,Z+			; String byte to rga, Z+
		cpi		rga,ctlZ		; byte ^Z?
		brne	ol_pptr2			; Print if not ^Z
		pop		rga
		ret
ol_pptr2:
		cpi		rga,NULL		; Skip any nulls in string
		breq	ol_pptr1
		push	ZH
		push	ZL
		rcall	ol_wrdat		; Get ol font pointer
		pop		ZL
		pop		ZH

		rjmp	ol_pptr1

;
;
; ==================================================================
;
; Write picture data to oled dram, horizontal addressing mode
;
; Write pix map to dram, horizontal addressing mode.	(OK)
;
ol_wr_hrz_dram:
		sbi		PORTB,ol_A0				; Data mode
;
		rcall	wr_pixmap_dram
;
		ret
;
; Write pixmap to dram using horizontal addressing mode	(OK)
;
wr_pixmap_dram:
		ldyptr	132*8			; Pixels to move
wr_pixmap_dram1:
		lpm		rga,Z+			; Fetch data from program memory
		rcall	ol_putb
		sbiw	YH:YL,1			; Count moves until done
		brne	wr_pixmap_dram1
		ret
;
; ==================================================================
;
; Clear oled dram
;
;
ol_clr_dram:
		clr		rmp					; Start at page 0
		sts		pageb,rmp			; Save to buffer
		ldi		YL,8				; All 8 pages
ol_clr_dram0:
		ldi		YH,132				; Columns per page
		rcall	set_page_addr		; Write page address to oled
;
; Setup oled dram col address
;
		cbi		PORTB,ol_A0			; Command mode
		ldi		rga,ol_set_lo_col	; Col 0 lo
		rcall	ol_putb				; Send rga command to oled
		ldi		rga,ol_set_hi_col	; Col 0 hi
		rcall	ol_putb				; Send rga command to oled
		sbi		PORTB,ol_A0			; Data mode
;
ol_clr_dram1:
		clr		rga
		rcall	ol_putb
		dec		YH					; Count columns written
		brne	ol_clr_dram1
		dec		YL					; Count pages
		brne	ol_clr_dram0
		ret
;
; ==================================================================
;
; Set oled display on/off		(OK)
;
ol_disp_on:
		cbi		PORTB,ol_A0			; Command mode
		ldi		rga,0xaf
		rcall	ol_putb				; Send rga command to oled
		sbi		PORTB,ol_A0			; Data mode
		ret
;
ol_disp_off:
		cbi		PORTB,ol_A0			; Command mode
		ldi		rga,0xae
		rcall	ol_putb				; Send rga command to oled
		sbi		PORTB,ol_A0			; Data mode
		ret
;
;
;========================
; Initialize OLED module
;========================
;
; Configure the SSD1306 OLED display controller via 4-wire SPI bus
; using m328P SPI hardware.
;
; Enable OLED module by ol_cs = 0. Some modules don't have CS wired out.
; The initialization procedure follows the manufacturer's recommendations.
;
;
oled_config:
		sbi		PORTB,ol_cs				; Disable SPI communications
		cbi		PORTB,ol_A0				; Command mode

		sbi		PORTB,ol_RST			; Reset = 1
		rcall	d_100m					; 100 ms delay
		cbi		PORTB,ol_RST			; Reset line = 0
		rcall	d_100m					; 100 ms delay
		sbi		PORTB,ol_RST			; Reset = 1

		cbi		PORTB,ol_cs				; Enable SPI communications
; 1
		ldi		rga,ol_set_display_off	; Display OFF
		rcall	ol_putb					; Send rga command to oled
; 2
		ldi		rga,ol_set_mux_ratio	; Multiplex ratio
		rcall	ol_putb					; Send rga command to oled
		ldi		rga,0x3f				; 0x7f is default value
		rcall	ol_putb
; 3
		ldi		rga,ol_set_display_offset	; Vertical shift
		rcall	ol_putb
		ldi		rga,0x0					; 0x7f is default value
		rcall	ol_putb
; 4
		ldi		rga,ol_set_start_line	; Display start line
		rcall	ol_putb
; 5
		ldi		rga,ol_set_segment_remap1	; COL127 mapped to SEG0
		rcall	ol_putb
; 6
		ldi		rga,ol_set_com_scan_dir	; Scan direction set to reverse
		rcall	ol_putb
; 7
		ldi		rga,ol_set_com_config	; COM pin HW configuration
		rcall	ol_putb
		ldi		rga,0x12				; Alternative pin configuration
		rcall	ol_putb
; 8
		ldi		rga,ol_set_contrast		; Contrast control
		rcall	ol_putb
		ldi		rga,0x4f				; 0x7f is default
		rcall	ol_putb
; 9
		ldi		rga,ol_set_precharge	; Pre-charge period
		rcall	ol_putb
		ldi		rga,0xf1				; 0x7f is default
		rcall	ol_putb
; 10
		ldi		rga,ol_set_Vcomh		; Vcomh regulator output
		rcall	ol_putb
		ldi		rga,0x40				; 0x7f is default
		rcall	ol_putb
; 11
		ldi		rga,ol_set_no_scroll	; Deactivate scrolling
		rcall	ol_putb

; 12
		ldi		rga,ol_set_whole_display_on	; Whole display ON
		rcall	ol_putb
; 13
		ldi		rga,ol_set_normal		; Normal display mode
		rcall	ol_putb
; 14
		ldi		rga,ol_set_enable_charge_pmp	; Contrast control
		rcall	ol_putb
		ldi		rga,0xcf				; 0x7f is default
		rcall	ol_putb
; 15
		ldi		rga,ol_set_mem_addr		; Memory addressing mode
		rcall	ol_putb
		ldi		rga,0x0					; Horizontal addressing mode
		rcall	ol_putb
; 16
		ldi		rga,ol_set_normal		; Normal display mode
		rcall	ol_putb
; 17
		ldi		rga,ol_set_display_on	; Display ON
		rcall	ol_putb
;
		sbi		PORTB,ol_A0				; Data mode
		ret
;
;
; Write data to OLED slave module. Sends 8 bit byte to SPI 	(OK)
;
; Registers:	rga, rmp
; Entry:		rga <-- data to send
;
ol_putb:
		out		SPDR,rga		; Send via SPI bus
ol_putb1:
		in		rmp,SPSR
		sbrs	rmp,SPIF		; Wait for transmission complete
		rjmp	ol_putb1
		ret
;
;
;
;###########################################################################
;
;
; Convert GMT time to Local time by adjusting hours to time zone difference.
; Read in the hours ascii characters from timeb (first two digits), convert
; to packed BCD number. Add/subtract TZoff by inc/dec hours BCD by time zone
; amount. Convert result back to ascii chars and store in LT_hourb buffer.
;
; (See m328-Simple-RTC.asm for set_clk_hr example code)
;
gmt_lt_hr:
		ldyptr	LT_hourb
		lds		XH,timeb			; Get timeb tens hours
		ldi		rmp,'0'				; Ascii '0'
		sub		XH,rmp				; Convert to binary
		swap	XH					; Position as high BCD nibble
		lds		XL,(timeb+1)		; Get timeb units hours
		sub		XL,rmp				; Convert to binary, low nibble
		or		XH,XL				; Merge to form packed BCD byte in XH
		mov		rga,XH				; Packed BCD hours in rga
		lds		rgb,TZb				; Get TZ offset
		tst		rgb					; Test TZ offset value for 0
		breq	gmt_lt_hr3			;	Yes, no correction needed
;
;
; Check for sign of TZ correction
;
		sbrs	flaga,TZposf		; Check if + or - TZ
		rjmp	dec_hr				;	No, -ve TZ
;
; Arrive here if in +ve TZ correction
;
inc_hr:
		inc		rga
		rcall	daa
		cpi		rga,0x24			; Rollover at 24 h
		brne	gmt_lt_hr2
		clr		rga					; Clear to 00
;
gmt_lt_hr2:
		dec		rgb					; Apply time zone correction
		brne	inc_hr
;
; Apply DST correction if DST_en pin grounded
;
		sbis	PINC,DST_en			; Test if DST_en pin open
		inc		rga					;	No, grounded, apply DST correction
		rjmp	gmt_lt_hr3
;
; Arrive here if in -ve TZ correction
;
dec_hr:
		tst		rga					; Test if hours = 00
		breq	dec_hr1				;	Yes, rollunder to 23 hours
		dec		rga
		rcall	das
		rjmp	gmt_lt_hr1
dec_hr1:
		ldi		rga,0x23
;
gmt_lt_hr1:
		dec		rgb					; Apply time zone correction
		brne	dec_hr
;
; Apply DST correction if DST_en pin grounded
;
		sbis	PINC,DST_en			; Test if DST_en pin open
		inc		rga					;	No, grounded, apply DST correction
;
; Convert packed BCD number (2 digits) into two ascii digits
;
gmt_lt_hr3:
		push	rga					; Convert packed BCD to ascii
		swap	rga					; Process MSD nibble first
		rcall	cv2asc				; Convert to ascii
		pop		rga
;
; Convert low nibble to ascii digit, store to LT_hourb buffer
;
cv2asc:
		andi	rga, 0x0f			; Mask off higher nibble
		ldi		rgv, 0x30 			; Add ascii '0' to convert
		add		rga, rgv			; Convert to ascii in rga
		st		Y+,rga				; Update LT_hourb buffer
		ret
;
;
; --- Decimal Adjust after Addition ---
;
; rga has result of 2 packed packed BCD digits after addition on entry.
; DAA routine adjusts rga for proper BCD representation. C flag has result
; of carry-out to allow for multiple precision additions.
;
; Registers: rga
; Entry:	rga = result of prior packed bcd addition
; Exit:		rga = decimal adjusted result for 2 digits
;
daa:
		push	rmp
		ldi		rmp,0x66	; Adjustment for both BCD digits
		add		rga,rmp		; Add adjustment to BCD pair
		brcc	danoc		; C=0
		andi	rmp,0x0f	; C=1, high nibble adjustment removed
danoc:
		brhc	danoh		; H=0
		andi	rmp,0xf0	; H=1, low nibble adjustment removed
danoh:
		sub		rga,rmp		; Final adjustment
		pop		rmp
		ret
;
; DAS decimal adjust subtraction of two packed BCD numbers
; Based on Intel IA-32 instruction code for DAS
;
das:
		push	rgb					; Save working registers
		push	rgc
;
; Save entry data
;
		mov		rgb,rga				; copy of rga for lo nibble testing
		mov		rgc,rga				; rgc has rga for hi nibble testing
;
; Test low nibble. If ((rga & 0x0f) > 9)) --> daa_adjlo
;
das_testlo:
		andi	rgb,0x0f
		cpi		rgb,9+1
		brcc	das_adjlo		; Low nibble is > 9
		rjmp	das_testhi
;
das_adjlo:
		ldi		rgb,0x06		; Decimal adjust for low nibble
		sub		rga,rgb			; Add 0x06 to entry_rga
;
; Test high nibble. If ((entry_rga > 0x99) or (entry_C = 1)) --> daa_adjhi
;
das_testhi:
		cpi		rgc,0x99+1		; Test entry_rga
		brcc	das_adjhi		; entry_rga > 0x99
		rjmp	das_x
;
das_adjhi:
		ldi		rgb,0x60		; Decimal adjust for high nibble
		sub		rga,rgb			; Add 0x60 to entry_rga
;
das_x:
;
		pop		rgc
		pop		rgb
		ret
;
;
;
;
;###########################################################################
;
;
; --- Message scan routines ---	(OK)
;
;
;	Registers used:
;	rmp, rga, rgb, rgc, ZHL
;
; Exit: C = 1 if fail,
;		C = 0 if success
;
; Desired sentence type ($GPRMC), has time and date information
;
scn_hdr:
		rcall	ci				; Get a character
		cpi		rga,'$'			; Test if '$'
		brne	scn_hdr			;	No, wait '$'
;
; Incoming characters following '$'in rga
;
		ldzptr	RMC_hdr			; Header string pointer
		ldi		rgb,hdr_sz		; Length of header string
;
scn_hdr0:
		rcall	ci				; Get next character
		lpm		rgc,Z+			; Get a RMC_hdr string character
;
; Check for match
;
		eor		rga,rgc			; Match?
		breq	scn_hdr1		; Yes
		sec						; No, error exit
		ret
;
; Check remaining characters
;
scn_hdr1:
		dec		rgb				; Count characters compared
		brne	scn_hdr0
		clc
		ret						; Exit match found
;
; GPRMC header string
;
RMC_hdr:
		.db		"GPRMC,",ctlZ	; Sentence header
;
;
;
;###########################################################################
;
; Timers - general purpose
;
;
; 100 ms delay using t4ms tick flag	(OK)
;
d_100m:
		ldi		rgc,24				; Counter for 4.096 ms ticks
		cbr		flaga,(1<<t4msf)	; Clear tick flag
d_100m1:
		sbrs	flaga,t4msf
		rjmp	d_100m1				; Wait for flag = 1
		cbr		flaga,(1<<t4msf)
		dec		rgc
		brne	d_100m1
		ret
;
;
; Zero RAM data space
;
zbuf:
		ldi		rgb,(enbuf-stbuf)
		ldxptr	stbuf			; Start at linbuf
		clr		rmp
zbuf1:
		st		X+,rmp
		dec		rgb
		brne	zbuf1
		ret
;
; Zero lower registers R0...R15
;
zregs:
		ldi		rga,16
		clr		rmp
		ldxptr	0x0			; Register file base address
zregs1:
		st		X+,rmp
		dec		rga
		brne	zregs1
		ret
;
;
;###########################################################################
;
;
; "div8u" - 8/8 Bit Unsided Division				(OK)
;
; This subroutine divides the two register variables "rga" (dividend) and
; "rgb" (divisor). The result is placed in "rga" and the remainder in "rgb".
;
; High registers used:	4 (rga,rgb,rgc,rgv)
;
;
; Register Variables:
;	rgc	remainder
;	rga	dividend & result
;	rgb divisor
;	rgv	loop counter
;
; Entry:	(rga) = dividend
;			(rgb) = divisor
; Exit:		(rga) = integer part of quotient
;			(rgb) = integer remainder
;
div8u:
		push	rgc
		push	rgv
		sub		rgc,rgc			; clear remainder and carry
        ldi		rgv,9			; init loop counter
d8u_1:	rol		rga				; shift left dividend
        dec		rgv				; decrement counter
        brne	d8u_2			; if done
		mov		rgb,rgc			; move remainder to rgb
		pop		rgv
		pop		rgc
        ret						;    return
;
d8u_2:	rol		rgc				; shift dividend into remainder
        sub		rgc,rgb			; remainder = remainder - divisor
        brcc	d8u_3			; if result negative
        add		rgc,rgb			;    restore remainder
        clc						;    clear carry to be shifted into result
        rjmp	d8u_1			; else
d8u_3:	sec						;    set carry to be shifted into result
        rjmp	d8u_1
;
;
;###########################################################################
;
;
; --- Low level video drivers ---
;
; Register rga used to pass data to console output routine
;
; Print rga data as two hexadecimal digits.			(OK)
;
pahex:
	push	rga
	swap	rga				; Show MSD nibble first
	rcall	pahex1
	pop		rga
pahex1:
	andi	rga, 0x0f		; Mask off higher nibble
	ldi		rgv, 0x30 		; Add ascii '0' to convert
	add		rga, rgv		; Convert to ascii
	cpi		rga, 0x3a		; Check if > 9
	brcs	pahex2			;  No, it is 0 ... 9
	ldi		rgv, 0x07		;  Yes, convert to A ... F
	add		rga, rgv
pahex2:
	call	co
	ret
;
; Print rga contents as decimal (0...255). Leading			(OK)
; zero suppression is provided only on the 100's
; digit, so at least two digits are always printed.
;
; Registers rga, rgb not saved
;
pdec:
	ldi		rgb,100			; Get 100's digit
	call	div8u
	tst		rga				; Do leading zero suppression
	breq	pdec1
	call	pnum
pdec1:
	ldi		rga,10			; Get 10's digit
	xchreg	rga,rgb
	call	div8u			; rgb has units
	call	pnum
	xchreg	rga,rgb
pnum:
	ori		rga,0x30		; Ascii "0"
	call	co				; Show ascii decimal
	ret
;
; Scan for keyboard input and return char in rga if any,
; else rga=0.
;
getc:
	lds		rmp,UCSR0A		; Get UART control status register
	sbrs	rmp,RXC0		; Test receiver complete flag
	rjmp	getc1
	lds		rga,UDR0		; rga <-- UDR0
	ret
getc1:
	clr	rga
	ret
;
; Load rga from UDR0 register. Waits until data byte is received.		(OK)
;
ci:
	lds		rmp,UCSR0A		; Get UART control status register
	sbrs	rmp,RXC0		; Test receiver complete flag
	rjmp	ci
;
; Fetch data
;
	lds		rga,UDR0		; rga <-- UDR0
	ret
;
; Load UDR0 from rga. Wait until transmitter is empty before loading.		(OK)
;
co:
	lds		rmp,UCSR0A		; Get UART control status register
	sbrs	rmp,UDRE0		; Test if UDR0 is empty
	rjmp	co
;
; Send data
;
	sts		UDR0,rga		; UDR0 <-- rga
	ret
;
; Print CR and LFs	(OK)
;
crllf:
	rcall	crlf			; Two CRLF
crlf:
	push	rga
	ldi		rga,CR			; Carriage return
	call	co
	ldi		rga,LF			; Linefeed
	rjmp	cco
;
; Print spaces	(OK)
;
dblsp:
	call	space
space:
	push	rga
	ldi		rga,SP			; Space
cco:
	call	co
	pop		rga
	ret
;
; Print comma	(OK)
;
prcma:
	push	rga
	ldi		rga,cma
	rjmp	cco
;
; Print message string, ^Z terminated. Routine is called with		(OK)
; code address of string loaded in ZH:ZL.
;
pptr:
	push	rga
pptr1:
	lpm		rga,Z+			; String byte to rga, Z+
	cpi		rga,ctlZ		; byte ^Z?
	brne	pptr2			; Print if not ^Z
	pop		rga
	ret
pptr2:
	cpi		rga,NULL		; Skip any nulls in string
	breq	pptr1
	rcall	co
	rjmp	pptr1
;
;
;
;###########################################################################
;
;
; Font bit map data
;
/*
 *
 * Verdana_digits_24
 *
 * created with FontCreator
 * written by F. Maximilian Thiele
 *
 * http://www.apetech.de/fontCreator
 * me@apetech.de
 *
 * File Name           : Verdana_digits_24
 * Date                : 01.05.2008
 * Font size in bytes  : 3833
 * Font width          : 10
 * Font height         : 24
 * Font first char     : 48
 * Font last char      : 59
 * Font used chars     : 11
 *
 * The font data are defined as
 *
 * struct _FONT_ {
 *     uint16_t   font_Size_in_Bytes_over_all_included_Size_it_self;
 *     uint8_t    font_Width_in_Pixel_for_fixed_drawing;
 *     uint8_t    font_Height_in_Pixel_for_all_characters;
 *     unit8_t    font_First_Char;
 *     uint8_t    font_Char_Count;
 *
 *     uint8_t    font_Char_Widths[font_Last_Char - font_First_Char +1];
 *                  // for each character the separate width in pixels,
 *                  // characters < 128 have an implicit virtual right empty row
 *
 *     uint8_t    font_data[];
 *                  // bit field of all characters
 */

; VERDANA24_WIDTH 17
; VERDANA24_HEIGHT 24

;
.dw		0		; Align table on word boundary
;
; Note: The font data table must be contiguous words (2 bytes) with no padding bytes added
; between .db declarations. Ensure that all .db lines contain even number of bytes to avoid
; addition of padding bytes by assembler
;

fontdata:
;
;
; GLCDFONTDECL(Verdana_digits_24)
;
.db		0x0E,0xF9,0x11,0x18,0x30,0x0B
;
; Font header: size16,width8,height8,first_char8,char_count8
;
.db		0x10, 0x0D, 0x0F, 0x0F, 0x11, 0x0F, 0x10, 0x10, 0x10, 0x10, 0x04, 0x80, 0xF0, 0xFC, 0x7E, 0x0E
.db		0x0F, 0x07, 0x07, 0x07, 0x07, 0x0F, 0x1E, 0x7E, 0xFC, 0xF0, 0x80, 0xFF, 0xFF, 0xFF, 0x00, 0x00
.db		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x01, 0x0F, 0x3F, 0x7E, 0x70
.db		0xF0, 0xE0, 0xE0, 0xE0, 0xE0, 0xF0, 0x70, 0x7E, 0x3F, 0x0F, 0x01, 0x38, 0x38, 0x38, 0x38, 0x3C
.db		0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF
.db		0x00, 0x00, 0x00, 0x00, 0x00, 0xE0, 0xE0, 0xE0, 0xE0, 0xE0, 0xFF, 0xFF, 0xFF, 0xE0, 0xE0, 0xE0
.db		0xE0, 0xE0

.db		0x00, 0x1E, 0x0E, 0x0E, 0x07, 0x07, 0x07, 0x07, 0x07, 0x0F, 0x1E, 0xFE, 0xFC, 0xF0, 0x00, 0x00
.db		0x00, 0x00, 0x00, 0x00, 0x80, 0xC0, 0xE0, 0xF0, 0x78, 0x3E, 0x1F, 0x07, 0x01, 0x00, 0xF0, 0xF8
.db		0xFC, 0xFE, 0xEF, 0xE7, 0xE3, 0xE1, 0xE0, 0xE0, 0xE0, 0xE0, 0xE0, 0xE0, 0xE0, 0x00, 0x1E, 0x0E
.db		0x0E, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x0F, 0x1E, 0xFE, 0xFC, 0xF8, 0x00, 0x00, 0x00, 0x00
.db		0x00, 0x1C, 0x1C, 0x1C, 0x1C, 0x1E, 0x36, 0x77, 0xF3, 0xE1, 0xC0, 0x78, 0x70, 0x70, 0xF0, 0xE0
.db		0xE0, 0xE0, 0xE0, 0xE0, 0xF0, 0x70, 0x78, 0x3F, 0x1F, 0x0F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80
.db		0xC0, 0xF0, 0xF8, 0x7C, 0x1E, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0xE0, 0xF0, 0xFC, 0xFE, 0xDF
.db		0xC7, 0xC3, 0xC1, 0xC0, 0xC0, 0xC0, 0xFF, 0xFF, 0xFF, 0xC0, 0xC0, 0xC0, 0x01, 0x01, 0x01, 0x01
.db		0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0xFF, 0xFF, 0xFF, 0x01, 0x01, 0x01, 0x00, 0xFF, 0xFF
.db		0xFF, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x00, 0x1F, 0x1F, 0x0F
.db		0x0E, 0x0E, 0x0E, 0x0E, 0x0E, 0x1E, 0x1C, 0x3C, 0xF8, 0xF8, 0xE0, 0x78, 0x70, 0x70, 0xF0, 0xE0
.db		0xE0, 0xE0, 0xE0, 0xE0, 0xF0, 0x70, 0x7C, 0x3F, 0x1F, 0x07

.db		0x00, 0xC0, 0xF0, 0xF8, 0x3C, 0x1E, 0x0E, 0x0E, 0x07, 0x07, 0x07, 0x07
.db		0x07, 0x0F, 0x00, 0x00, 0xFE, 0xFF, 0xFF, 0x1C, 0x1C, 0x0E, 0x0E, 0x0E
.db		0x0E, 0x0E, 0x0E, 0x1E, 0x3C, 0xF8, 0xF8, 0xE0, 0x03, 0x0F, 0x3F, 0x7C
.db		0x78, 0xF0, 0xE0, 0xE0, 0xE0, 0xE0, 0xE0, 0x70, 0x78, 0x3F, 0x1F, 0x07
.db		0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x07, 0x87
.db		0xE7, 0xFF, 0x7F, 0x1F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xC0
.db		0xF0, 0xFC, 0x3F, 0x0F, 0x07, 0x01, 0x00, 0x00, 0x00, 0x00, 0x80, 0xE0
.db		0xF8, 0xFC, 0x3F, 0x0F, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00

.db		0x00, 0xF0, 0xFC, 0xFE, 0x0E, 0x0F, 0x07, 0x07, 0x07, 0x07, 0x0F, 0x1E
.db		0xFE, 0xFC, 0xF8, 0x00, 0x80, 0xE0, 0xF3, 0x77, 0x1F, 0x0E, 0x0E, 0x0C
.db		0x1C, 0x1C, 0x1C, 0x3E, 0x77, 0xF3, 0xE0, 0x80, 0x0F, 0x1F, 0x3F, 0x78
.db		0x70, 0xF0, 0xE0, 0xE0, 0xE0, 0xE0, 0xE0, 0x70, 0x78, 0x3F, 0x1F, 0x0F

.db		0xE0, 0xF8, 0xFC, 0x1E, 0x0E, 0x07, 0x07, 0x07, 0x07, 0x07, 0x0F, 0x1E
.db		0x3E, 0xFC, 0xF0, 0xC0, 0x07, 0x1F, 0x1F, 0x3C, 0x78, 0x70, 0x70, 0x70
.db		0x70, 0x70, 0x70, 0x38, 0x38, 0xFF, 0xFF, 0x7F, 0x00, 0x00, 0xF0, 0xE0
.db		0xE0, 0xE0, 0xE0, 0xE0, 0x70, 0x70, 0x78, 0x3C, 0x1F, 0x0F, 0x03, 0x00

.db		0xC0, 0xC0, 0xC0, 0xC0, 0x03, 0x03, 0x03, 0x03, 0xF0, 0xF0, 0xF0, 0xF0
;
fontdata_end:
;
;
;
;
;###########################################################################
;
;
.exit
;
;
; --- End of source code ---
;
;

