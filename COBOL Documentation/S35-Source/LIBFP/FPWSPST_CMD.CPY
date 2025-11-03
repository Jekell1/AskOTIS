      * COPYMEMBER: LIBFP/FPWSPST.CMD
      ******************************************************************
      *
      * IDENTIFICATION:  LIBFP/FPWSPST.CMD
      *
      * DESCRIPTION   :  'WS' (.CMD) FOR PST-FILE
      *
      *=================================================================
      * REV:
      * KEC 2011-0923 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               ***NEW***  CREATED FROM LIBLP/LASER.CMD
      *
      * KEC 2020-0722 {PR#00000} WORLD <A15> [JAY CISZEWSKI]
      *               DUE TO '35PPM PS' PRINT DRIVER NOT WORKING
      *               WITH LATEST WINDOWS 10 UPDATE, NEEDED TO MAKE
      *               CHANGES TO USE 'HP UNIVERSAL PRINTING PS'
      *               PRINT DRIVER WITH NEW /usr/A##/PST FILES:
      *                  ADDED PST-WS-FORM-BEGPAGE.
      *                  CHANGED PST-WS-FORM-ENDPAGE.
      *
      * KEC 2020-0828 {PR#00000} WORLD <A15> [JAY CISZEWSKI]
      *               PER APPROVAL FROM KEVIN MULLEN, COMMENTED OUT
      *               USE OF PST-WS-RM-COMMAND-SWITCH; ADDED USE OF
      *               PST-WS-OUTPUT-SWITCH WITH OPTIONS OF N, P & S.
      *
      * KEC 2020-1014 {PR#00000} WORLD <A15> [JAY CISZEWSKI]
      *               ADDED THE FOLLOWING TO CREATE PDF FILES USING
      *               GHOSTSCRIPT (ps2pdf) FOR DEVELOPMENT:
      *                 03  PST-PATH-PDF-EXT
      *                 03  PST-PATH-DEV
      *                 88  PST-WS-OUTPUT-SWITCH-DEV
      *                 01  PST-WS-PS2PDF-COMMAND.
      *
      * KEC 2020-1020 {PR#00000} WORLD <A15> [JAY CISZEWSKI]
      *               ADDED THE FOLLOWING SO TO WORK WITH THE OLD
      *               '35PPM PS' PRINT DRIVER IN CRYSTAL REPORTS:
      *                 01  PST-WS-FORM-ENDPAGE-35PPM.
      *
      * KEC 2020-1029 {PR#00000} WORLD <A15> [JAY CISZEWSKI]
      *               COMMENTED OUT '01  PST-WS-FORM-ENDPAGEDUPLEX.';
      *               WORLD DOES NOT USE DUPLEX AND IT WAS BEING SLIPPED
      *               INTO LIVE PRODUCTION BY COPY EXISTING PROGRAMS
      *               FROM PFS CUSTOMERS AS A STARTER.
      * AMM 2021-0218 {PR#00000} WORLD <A15> [ASHLEY MILEWSKI]
      *               ADDED PST-DRIVER-SWITCH
      *
      * JKC 2022-0602 REPLACED THE USE OF COURIER WITH NIMBUS MONO;
      *         AS OF JANUARY 2023, TYPE 1 FONTS WERE NO LONGER GOING
      *         TO BE SUPPORTED OF WHICH COURIER WAS ONE OF THE THEM.
      *         NIMBUS MONO WAS THE EQUILVENT TO COURIER.
      * BAH 2023-1110 ADDED SEVERAL ITEMS FOR F3-ALL DOCUMENT PRINT TO FILE
      *               WITH MANIFEST #1630
      *
      * JKC 2025-0206 REMOVED PST-WS-SSH-SCRIPT WORKERS SINCE NOT USED.
      *
      ******************************************************************
      *
      *    P S T - C M D - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/HEX_CMD.CPY".
      *=================================================================
       01  PST-CMD-CRLF.
           03  PST-CMD-CRLF-CR         PIC X(01)     VALUE X"0A".
           03  PST-CMD-CRLF-LF         PIC X(01)     VALUE X"0D".

       78  PST-CMD-LINEFEED                          VALUE X"0D".
       78  PST-CMD-MOVETO                            VALUE "moveto".
       78  PST-CMD-ROTATE                            VALUE "rotate".
       78  PST-CMD-SELECTFONT                        VALUE "selectfont".
       78  PST-CMD-SHOW                              VALUE ") show".
       78  PST-CMD-SHOWPAGE                          VALUE "showpage".
       78  PST-CMD-TRANSLATE                         VALUE "translate".

      ******************************************************************
      *
      *    P S T - W S - F O N T - W O R K E R S
      *
      ******************************************************************
       77  PST-WS-FONT-CODE            PIC X(50)     VALUE SPACES.
       77  PST-WS-FONT-SIZE            PIC 99.99     VALUE ZEROES.
       77  PST-WS-FONT-TYPE            PIC 9(02)     VALUE ZEROES.
       77  PST-WS-LINE-SPACING-SIZE    PIC 9(02)V99  VALUE 6.

      *=================================================================
       78  PST-WS-FONT-AVANTGARDE-B                  VALUE 01.
       78  PST-WS-FONT-AVANTGARDE-BO                 VALUE 02.
       78  PST-WS-FONT-AVANTGARDE-D                  VALUE 03.
       78  PST-WS-FONT-AVANTGARDE-DO                 VALUE 04.
       78  PST-WS-FONT-BOOKMAN-D                     VALUE 05.
       78  PST-WS-FONT-BOOKMAN-DI                    VALUE 06.
       78  PST-WS-FONT-BOOKMAN-L                     VALUE 07.
       78  PST-WS-FONT-BOOKMAN-LI                    VALUE 08.
       78  PST-WS-FONT-COURIER                       VALUE 09.
       78  PST-WS-FONT-COURIER-O                     VALUE 10.
       78  PST-WS-FONT-COURIER-B                     VALUE 11.
       78  PST-WS-FONT-COURIER-BO                    VALUE 12.
       78  PST-WS-FONT-HELVETICA                     VALUE 13.
       78  PST-WS-FONT-HELVETICA-O                   VALUE 14.
       78  PST-WS-FONT-HELVETICA-B                   VALUE 15.
       78  PST-WS-FONT-HELVETICA-N                   VALUE 16.
       78  PST-WS-FONT-HELVETICA-NB                  VALUE 17.
       78  PST-WS-FONT-HELVETICA-NBO                 VALUE 18.
       78  PST-WS-FONT-HELVETICA-NO                  VALUE 19.
       78  PST-WS-FONT-NEWCENTURY                    VALUE 20.
       78  PST-WS-FONT-NEWCENTURY-B                  VALUE 21.
       78  PST-WS-FONT-NEWCENTURY-I                  VALUE 22.
       78  PST-WS-FONT-NEWCENTURY-BI                 VALUE 23.
       78  PST-WS-FONT-PALATINO                      VALUE 24.
       78  PST-WS-FONT-PALATINO-B                    VALUE 25.
       78  PST-WS-FONT-PALATINO-I                    VALUE 26.
       78  PST-WS-FONT-PALATINO-BI                   VALUE 27.
       78  PST-WS-FONT-SYMBOL                        VALUE 28.
       78  PST-WS-FONT-TIMES                         VALUE 29.
       78  PST-WS-FONT-TIMES-I                       VALUE 30.
       78  PST-WS-FONT-TIMES-B                       VALUE 31.
       78  PST-WS-FONT-TIMES-BI                      VALUE 32.
       78  PST-WS-FONT-ZAPF                          VALUE 33.

      *-----------------------------------------------------------------
       78  PST-WS-FONT-TYPE-MAX                      VALUE 33.

      *====================================================[ FONT TYPE ]
       01  PST-WS-FONT-TYPE-TABLE.

      *- - - - - - - - - - - - - - - -{ AVANTGARDE-B              }-{01}
           03  FILLER PIC X(30) VALUE "/AvantGarde-Book              ".

      *- - - - - - - - - - - - - - - -{ AVANTGARDE-BO             }-{02}
           03  FILLER PIC X(30) VALUE "/AvantGarde-BookOblique       ".

      *- - - - - - - - - - - - - - - -{ AVANTGARDE-D              }-{03}
           03  FILLER PIC X(30) VALUE "/AvantGarde-Demi              ".

      *- - - - - - - - - - - - - - - -{ AVANTGARDE-DO             }-{04}
           03  FILLER PIC X(30) VALUE "/AvantGarde-DemiOblique       ".

      *- - - - - - - - - - - - - - - -{ BOOKMAN-D                 }-{05}
           03  FILLER PIC X(30) VALUE "/Bookman-Demi                 ".

      *- - - - - - - - - - - - - - - -{ BOOKMAN-DI                }-{06}
           03  FILLER PIC X(30) VALUE "/Bookman-DemiItalic           ".

      *- - - - - - - - - - - - - - - -{ BOOKMAN-L                 }-{07}
           03  FILLER PIC X(30) VALUE "/Bookman-Light                ".

      *- - - - - - - - - - - - - - - -{ BOOKMAN-LI                }-{08}
           03  FILLER PIC X(30) VALUE "/Bookman-LightItalic          ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * 2022-0602 JKC - While testing Postscript Type 3 fonts, I found
      * that the following fonts worked (was replacing the Courier
      * to test what worked):
      *    03  FILLER PIC X(30) VALUE "/Arial                        ".
      *    03  FILLER PIC X(30) VALUE "/Bodoni                       ".
      *    03  FILLER PIC X(30) VALUE "/Clarendon                    ".
      *    03  FILLER PIC X(30) VALUE "/Coronet                      ".
      *    03  FILLER PIC X(30) VALUE "/Eurostile-Bold               ".
      *    03  FILLER PIC X(30) VALUE "/Monaco                       ".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * 2022-0602 JKC - Replaced the use of Courier with Nimbus Mono;
      * As of January 2023, Type 1 fonts were no longer going to be
      * supported of which Courier was one of the them.  Nimbus Mono
      * was the equilvent to Courier.
      *- - - - - - - - - - - - - - - -{ COURIER                   }-{09}
      *    03  FILLER PIC X(30) VALUE "/Courier                      ".
           03  FILLER PIC X(30) VALUE "/NimbusMonoPS-Regular         ".

      *- - - - - - - - - - - - - - - -{ COURIER-O                 }-{10}
      *    03  FILLER PIC X(30) VALUE "/Courier-Oblique              ".
           03  FILLER PIC X(30) VALUE "/NimbusMonoPS-Italic          ".

      *- - - - - - - - - - - - - - - -{ COURIER-B                 }-{11}
      *    03  FILLER PIC X(30) VALUE "/Courier-Bold                 ".
           03  FILLER PIC X(30) VALUE "/NimbusMonoPS-Bold            ".

      *- - - - - - - - - - - - - - - -{ COURIER-BO                }-{12}
      *    03  FILLER PIC X(30) VALUE "/Courier-BoldOblique          ".
           03  FILLER PIC X(30) VALUE "/NimbusMonoPS-BoldItalic      ".

      *- - - - - - - - - - - - - - - -{ HELVETICA                 }-{13}
           03  FILLER PIC X(30) VALUE "/Helvetica                    ".

      *- - - - - - - - - - - - - - - -{ HELVETICA-O               }-{14}
           03  FILLER PIC X(30) VALUE "/Helvetica-Oblique            ".

      *- - - - - - - - - - - - - - - -{ HELVETICA-B               }-{15}
           03  FILLER PIC X(30) VALUE "/Helvetica-Bold               ".

      *- - - - - - - - - - - - - - - -{ HELVETICA-N               }-{16}
           03  FILLER PIC X(30) VALUE "/Helvetica-Narrow             ".

      *- - - - - - - - - - - - - - - -{ HELVETICA-NB              }-{17}
           03  FILLER PIC X(30) VALUE "/Helvetica-Narrow-Bold        ".

      *- - - - - - - - - - - - - - - -{ HELVETICA-NBO             }-{18}
           03  FILLER PIC X(30) VALUE "/Helvetica-Narrow-BoldOblique ".

      *- - - - - - - - - - - - - - - -{ HELVETICA-NO              }-{19}
           03  FILLER PIC X(30) VALUE "/Helvetica-Narrow-Oblique     ".

      *- - - - - - - - - - - - - - - -{ NEWCENTURY                }-{20}
           03  FILLER PIC X(30) VALUE "/NewCenturySchlbk-Roman       ".

      *- - - - - - - - - - - - - - - -{ NEWCENTURY-B              }-{21}
           03  FILLER PIC X(30) VALUE "/NewCenturySchlbk-Bold        ".

      *- - - - - - - - - - - - - - - -{ NEWCENTURY-I              }-{22}
           03  FILLER PIC X(30) VALUE "/NewCenturySchlbk-Italic      ".

      *- - - - - - - - - - - - - - - -{ NEWCENTURY-BI             }-{23}
           03  FILLER PIC X(30) VALUE "/NewCenturySchlbk-BoldItalic  ".

      *- - - - - - - - - - - - - - - -{ PALATINO                  }-{24}
           03  FILLER PIC X(30) VALUE "/Palatino-Roman               ".

      *- - - - - - - - - - - - - - - -{ PALATINO-B                }-{25}
           03  FILLER PIC X(30) VALUE "/Palatino-Bold                ".

      *- - - - - - - - - - - - - - - -{ PALATINO-I                }-{26}
           03  FILLER PIC X(30) VALUE "/Palatino-Italic              ".

      *- - - - - - - - - - - - - - - -{ PALATINO-BI               }-{27}
           03  FILLER PIC X(30) VALUE "/Palatino-BoldItalic          ".

      *- - - - - - - - - - - - - - - -{ SYMBOL                    }-{28}
           03  FILLER PIC X(30) VALUE "/Symbol                       ".

      *- - - - - - - - - - - - - - - -{ TIMES                     }-{29}
           03  FILLER PIC X(30) VALUE "/Times-Roman                  ".

      *- - - - - - - - - - - - - - - -{ TIMES-I                   }-{30}
           03  FILLER PIC X(30) VALUE "/Times-Italic                 ".

      *- - - - - - - - - - - - - - - -{ TIMES-B                   }-{31}
           03  FILLER PIC X(30) VALUE "/Times-Bold                   ".

      *- - - - - - - - - - - - - - - -{ TIMES-BI                  }-{32}
           03  FILLER PIC X(30) VALUE "/Times-BoldItalic             ".

      *- - - - - - - - - - - - - - - -{ ZAPF                      }-{33}
           03  FILLER PIC X(30) VALUE "/Zapf-Chancery-MediumItalic   ".

      *-----------------------------------------------------------------
       01  FILLER REDEFINES PST-WS-FONT-TYPE-TABLE.

           03  PST-WS-FONT-TYPE-ELE
                      PIC X(30) OCCURS PST-WS-FONT-TYPE-MAX.

      ******************************************************************
      *
      *    P S T - P A T H - W O R K E R S
      *
      *=================================================================
      *    PST-PATH:  {DIRECTORY}-{FORM-PROGX}-{MKTEMP-NAME}-{PGNO}
      *    EXAMPLE :  /usr/tmp/PST###-??????-##
      ******************************************************************
       01  PST-PATH-WORKERS.
           03  PST-PATH                     PIC X(40) VALUE SPACES.
           03  PST-PATH-ALL                 PIC X(40) VALUE SPACES.
           03  PST-PATH-DEV                 PIC X(40) VALUE SPACES.
           03  PST-PATH-PRINT               PIC X(40) VALUE SPACES.
           03  PST-PATH-DIRECTORY-DEFAULT   PIC X(09) VALUE "/usr/tmp/".
           03  PST-PATH-DIRECTORY           PIC X(30) VALUE SPACES.

           03  PST-PATH-FORM-PROGX          PIC X(06) VALUE SPACES.
           03  FILLER             REDEFINES PST-PATH-FORM-PROGX.
               05  PST-PATH-FORM-PROGX-PRE  PIC X(03).
               05  PST-PATH-FORM-PROGX-POST PIC X(03).

           03  PST-PATH-MKTEMP              PIC X(26) VALUE SPACES.
           03  PST-PATH-PDF-EXT             PIC X(04) VALUE ".pdf".
           03  PST-PATH-PGNO                PIC 9(02) VALUE ZEROES.

       01  PST-DATA-PATH          PIC X(35).
       01  PST-FIL-PATH           REDEFINES PST-DATA-PATH.
           03  PST-FIL-BASE       PIC X(13).

      * THIS FILE WILL HOLD THE PATHNAMES OF ALL FILES THE API NEEDS
      * TO GRAB
       01  PST-PATH-MANIFEST.
           03  FILLER                  PIC X    VALUE "/".
           03  PST-PATH-MAN-FIL        PIC X(12).
           03  FILLER                  PIC X(13) VALUE "ftp/out/DOCS/".
           03  PST-PATH-MAN-BRNO       PIC 9(04).
           03  PST-PATH-MAN-ACCTNO     PIC 9(08).

      * THIS FILE WILL HOLD THE PATHNAMES OF ALL FILES THE API NEEDS
      * TO GRAB
       01  PST-PATH-MANIFEST-DONE.
           03  FILLER                  PIC X    VALUE "/".
           03  PST-PATH-MAN-D-FIL        PIC X(12).
           03  FILLER                  PIC X(14) VALUE "ftp/out/DOCS/M".
           03  PST-PATH-MAN-D-BRNO       PIC 9(04).
           03  PST-PATH-MAN-D-ACCTNO     PIC 9(08).

       01  PST-PATH-PRINT-ALL.
           03  FILLER                      PIC X    VALUE "/".
           03  PST-PATH-PRINT-ALL-FIL      PIC X(12).
           03  FILLER                      PIC X(13) VALUE
                                                     "ftp/out/DOCS/".
           03  PST-PATH-PRINT-ALL-PROGRAM  PIC X(3).
           03  PST-PATH-PRINT-ALL-DASH1    PIC X VALUE "-".
           03  PST-PATH-PRINT-ALL-BRNO     PIC 9(04).
           03  PST-PATH-PRINT-ALL-ACCTNO   PIC 9(08).
           03  PST-PATH-PRINT-ALL-DASH2    PIC X VALUE "-".
           03  PST-PATH-PRINT-ALL-FMNO     PIC 9(03).
           03  PST-PATH-PRINT-ALL-DASH3    PIC X VALUE "-".
           03  PST-PATH-PRINT-ALL-TYPE     PIC XX.

      * FOR APILNS, THEY WANTED THEM IN A SEPARATE DIR

      * THIS FILE WILL HOLD THE PATHNAMES OF ALL FILES THE API NEEDS
      * TO GRAB FROM APILNS AUTOMATION
       01  PST-PATH-MANIFEST-DONE-AUTO.
           03  FILLER                      PIC X    VALUE "/".
           03  PST-PATH-MAN-D-AUTO-FIL     PIC X(12).
           03  FILLER                      PIC X(19) VALUE
                                            "ftp/out/DOCS/AUTO/M".
           03  PST-PATH-MAN-D-AUTO-BRNO    PIC 9(04).
           03  PST-PATH-MAN-D-AUTO-ACCTNO  PIC 9(08).

       01  PST-PATH-PRINT-ALL-AUTO.
           03  FILLER                      PIC X    VALUE "/".
           03  PST-PATH-PRINT-ALL-AUTO-FIL      PIC X(12).
           03  FILLER                      PIC X(18) VALUE
                                           "ftp/out/DOCS/AUTO/".
           03  PST-PATH-PRINT-ALL-AUTO-PROGRAM  PIC X(3).
           03  PST-PATH-PRINT-ALL-AUTO-DASH1    PIC X VALUE "-".
           03  PST-PATH-PRINT-ALL-AUTO-BRNO     PIC 9(04).
           03  PST-PATH-PRINT-ALL-AUTO-ACCTNO   PIC 9(08).
           03  PST-PATH-PRINT-ALL-AUTO-DASH2    PIC X VALUE "-".
           03  PST-PATH-PRINT-ALL-AUTO-FMNO     PIC 9(03).
           03  PST-PATH-PRINT-ALL-AUTO-DASH3    PIC X VALUE "-".
           03  PST-PATH-PRINT-ALL-AUTO-TYPE     PIC XX.

      ******************************************************************
      *
      *    P S T - S E Q - P A T H - W O R K E R S
      *
      *=================================================================
      *    PST-PATH:  {DIRECTORY}-{FORM-PROGX}.{PGNO}
      *    EXAMPLE :  /usr/A32/PST/PST###.##
      ******************************************************************
       01  PST-SEQ-PATH.
           03  FILLER                       PIC x(01) VALUE "/".
           03  PST-SEQ-PATH-REL             PIC X(07) VALUE SPACES.
           03  FILLER                       PIC X(05) VALUE "/PST/".

           03  PST-SEQ-FORM-PROGX           PIC X(06) VALUE SPACES.
           03  FILLER             REDEFINES PST-SEQ-FORM-PROGX.
               05  PST-SEQ-FORM-PROGX-PRE   PIC X(03).
               05  PST-SEQ-FORM-PROGX-POST  PIC X(03).
           03  FILLER                       PIC x(01) VALUE ".".
           03  PST-SEQ-PATH-PGNO            PIC 9(02).

      ******************************************************************
      *
      *    P S T - W S - F O R M - W O R K E R S
      *
      ******************************************************************
       01  PST-WS-FORM-PAGE-MAX        PIC 9(02)     VALUE 01.

       01  PST-WS-FORM-BEGPAGE.
           05  FILLER                  PIC X(01) VALUE "/".
           05  PST-WS-FORM-BP-RELEASE  PIC X(07) VALUE "usr/A##".
           05  FILLER                  PIC X(05) VALUE "/PST/".
           05  PST-WS-FORM-BP-PROGRAM  PIC X(17) VALUE "PST-BEGPAGE-HP".

       01  PST-WS-FORM-ENDPAGE.
           05  FILLER                  PIC X(01) VALUE "/".
           05  PST-WS-FORM-EP-RELEASE  PIC X(07) VALUE "usr/A##".
           05  FILLER                  PIC X(05) VALUE "/PST/".
           05  PST-WS-FORM-EP-PROGRAM  PIC X(17) VALUE "PST-ENDPAGE-HP".

       01  PST-WS-FORM-ENDPAGE-35PPM.
           05  FILLER                  PIC X(01) VALUE "/".
           05  PST-WS-FORM-EP-RELEA35  PIC X(07) VALUE "usr/A##".
           05  FILLER                  PIC X(05) VALUE "/PST/".
           05  PST-WS-FORM-EP-PROGRAM  PIC X(17) VALUE "PST-ENDPAGE".

      *01  PST-WS-FORM-ENDPAGEDUPLEX.
      *    05  FILLER                  PIC X(01) VALUE "/".
      *    05  PST-WS-FORM-EPD-RELEASE PIC X(07) VALUE "usr/A##".
      *    05  FILLER                  PIC X(05) VALUE "/PST/".
      *    05  PST-WS-FORM-EPD-PROGRAM PIC X(17) VALUE
      *                                          "PST-ENDPAGEDUPLEX".

       01  PST-WS-FORM-FILE.
           05  FILLER                  PIC X(01) VALUE "/".
           05  PST-WS-FORM-RELEASE     PIC X(07) VALUE "usr/A##".
           05  FILLER                  PIC X(05) VALUE "/PST/".
           05  PST-WS-FORM-PROGRAM     PIC X(06) VALUE "PST###".
           05  FILLER                  PIC X(01) VALUE ".".
           05  PST-WS-FORM-PAGE-NO     PIC 9(02) VALUE ZEROES.

      ******************************************************************
      *
      *    P S T - W S - S Y S T E M - W O R K E R S
      *
      ******************************************************************
       01  PST-WS-CAT-COMMAND.

           03  PST-WS-CAT-CMD          PIC X(04)     VALUE "cat ".

           03  PST-WS-CAT-BUF.
               05  PST-WS-CAT-FILE-01  PIC X(30)     VALUE SPACES.
               05  PST-WS-CAT-ROUTER   PIC X(04)     VALUE SPACES.
               05  PST-WS-CAT-FILE-02  PIC X(30)     VALUE SPACES.

       01  PST-WS-SEQ-REC.
           05  FILLER                  PIC X(14).
           05  PST-WS-SEQ-HEADER       PIC X(22).
               88  PST-WS-DRIVER-SWITCH-HP     VALUE 
                                               "SET STRINGCODESET=UTF8".
               88  PST-WS-DRIVER-SWITCH-35PPM  VALUE 
                                               "COMMENT 35PPM PS 11.00".

      *-----------------------------------------------------------------
      *  SWITCH USED TO DETERMINE WHAT TO DO WITH THE OUTPUT FILES:
      *    PST-PATH-ALL   = /usr/tmp/PST###-xxxxxx-##
      *    PST-PATH-PRINT = /usr/tmp/PRT###-xxxxxx
      *    PST-PATH-DEV   = /usr/tmp/DEV###-yymmdd-HHMMSS.pdf
      *
      *  SWITCH OPTIONS:
      *    N = NORMAL (PRINT AND REMOVE FILES)
      *    D = DEVELOPMENT (CREATE PDF AND REMOVE FILES; NO PRINT)
      *    P = PRINT & SAVE FILES
      *    S = SAVE ONLY (NOTHING GOES TO PRINTER; SAVES FILES)
      *-----------------------------------------------------------------
       77  PST-WS-OUTPUT-SWITCH    PIC X(01)     VALUE "N".
           88  PST-WS-OUTPUT-SWITCH-NORMAL       VALUE "N".
           88  PST-WS-OUTPUT-SWITCH-DEV          VALUE "D".
           88  PST-WS-OUTPUT-SWITCH-PRINT        VALUE "P".
           88  PST-WS-OUTPUT-SWITCH-SAVE         VALUE "S" "A".
           88  PST-WS-OUTPUT-SWITCH-ALL          VALUE "A".
           88  PST-WS-OUTPUT-SWITCH-VALID        VALUE "N" "D" "P"
                                                       "S" "A".

      *-----------------------------------------------------------------
       01  PST-WS-PS2PDF-COMMAND.

           03  PST-WS-PS2PDF-CMD          PIC X(07)  VALUE "ps2pdf ".

           03  PST-WS-PS2PDF-BUF.
               05  PST-WS-PS2PDF-FILE-01  PIC X(25)  VALUE SPACES.
               05  FILLER                 PIC X(01)  VALUE SPACES.
               05  PST-WS-PS2PDF-FILE-02  PIC X(33)  VALUE SPACES.

      *=================================================================
      *78  PST-WS-RM-COMMAND-SWITCH---ON             VALUE SPACES.
      *78  PST-WS-RM-COMMAND-SWITCH---OFF            VALUE "X".
      *77  PST-WS-RM-COMMAND-SWITCH    PIC X(01)     VALUE SPACES.
      *    88  PST-WS-RM-COMMAND-SWITCH-ON           VALUE SPACES.
      *    88  PST-WS-RM-COMMAND-SWITCH-OFF          VALUE "X".
      *-----------------------------------------------------------------
       01  PST-WS-RM-COMMAND.

           03  PST-WS-RM-CMD           PIC X(03)  VALUE "rm ".

           03  PST-WS-RM-BUF.
               05  PST-WS-RM-FILE      PIC X(30)  VALUE SPACES.
               05  PST-WS-RM-ERROR     PIC X(15)  VALUE " 2> /dev/null".

      *=================================================================
       01  PST-WS-MV-COMMAND.
           03  PST-WS-MV-CMD           PIC X(03)  VALUE "mv ".
           03  PST-WS-MV-BUF.
               05  PST-WS-MV-FILE-1    PIC X(55)  VALUE SPACES.
               05  PST-WS-MV-FILE-2    PIC X(55)  VALUE SPACES.
               05  PST-WS-MV-ERROR     PIC X(15)  VALUE " 2> /dev/null".

       01  PST-WS-MV-MANIFEST-COMMAND.
           03  PST-WS-MV-MANI-CMD        PIC X(03)  VALUE "mv ".
           03  PST-WS-MV-MANI-BUF.
               05  PST-WS-MV-MANI-FILE-1 PIC X(50) VALUE SPACES.
               05  PST-WS-MV-MANI-FILE-2 PIC X(50) VALUE SPACES.
               05  PST-WS-MV-MANI-ERROR  PIC X(15) VALUE
                                                   " 2> /dev/null".
