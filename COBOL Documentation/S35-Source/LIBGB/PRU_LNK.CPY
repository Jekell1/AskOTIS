      ******************************************************************
      *
      * PRINT LINK WORKERS
      *
      *=================================================================
      * REV:
      * BAH 130424 MADE 00 ONLY VALID PRINTER PRU-VALID
      * BAH 130606 ADDED P-CALLING-DIR AND P-CALLING-PROG FOR HELP
      *
      * JKC 2020-1026 ADDED P-TIMER-LINK-BUF.  USED FOR GB/TIMER.CBL
      *
      * SMR 2024-0401 ADDED P-TIMER-BEG-DATE TO BE USED FOR CALCULATING
      *          THE ELAPSED TIME IF THE END TIME CROSS OVER TO A NEW
      *          DAY.
      *
      ******************************************************************
       01  PRU-LINK.
      * PRINT FILE OPTIONS
      *--------?-POS: SCREEN POSITIONS FOR PRINT FIELDS.  [000000 = NON-DISP]
           03  Q-POS          PIC 9(6).
           88  Q-NONDISP                    VALUE ZEROS.
           88  Q-POS-VALID  VALUE 001001 THRU 001079 002001 THRU 002079
                                  003001 THRU 003079 004001 THRU 004079
                                  005001 THRU 005079 006001 THRU 006079
                                  007001 THRU 007079 008001 THRU 008079
                                  009001 THRU 009079 010001 THRU 010079
                                  011001 THRU 011079 012001 THRU 012079
                                  013001 THRU 013079 014001 THRU 014079
                                  015001 THRU 015079 016001 THRU 016079
                                  017001 THRU 017079 018001 THRU 018079
                                  019001 THRU 019079 020001 THRU 020079
                                  021001 THRU 021079 022001 THRU 022079
                                  023001 THRU 023079 000000.
           03  Q-POSITION REDEFINES Q-POS.
               05  Q-LINE     PIC 9(03).
               88  Q-LINE-VALID             VALUE 1 THRU 23.
               05  Q-COL      PIC 9(03).
               88  Q-COL-VALID              VALUE 1 THRU 79.
           03  Q-BUF          PIC X.
           88  Q-BUF-VALID                  VALUE "P" "V" "N".

           03  C-POS          PIC 9(6).
           88  C-NONDISP                         VALUE ZEROS.
           88  C-POS-VALID  VALUE 001001 THRU 001079 002001 THRU 002079
                                  003001 THRU 003079 004001 THRU 004079
                                  005001 THRU 005079 006001 THRU 006079
                                  007001 THRU 007079 008001 THRU 008079
                                  009001 THRU 009079 010001 THRU 010079
                                  011001 THRU 011079 012001 THRU 012079
                                  013001 THRU 013079 014001 THRU 014079
                                  015001 THRU 015079 016001 THRU 016079
                                  017001 THRU 017079 018001 THRU 018079
                                  019001 THRU 019079 020001 THRU 020079
                                  021001 THRU 021079 022001 THRU 022079
                                  023001 THRU 023079 000000.
           03  C-POSITION REDEFINES C-POS.
               05  C-LINE     PIC 9(03).
               88  C-LINE-VALID             VALUE 1 THRU 23.
               05  C-COL      PIC 9(03).
               88  C-COL-VALID              VALUE 1 THRU 79.
           03  C-BUF          PIC X(1).
           88  C-BUF-VALID                  VALUE "0" "1" "2" "3" "4"
                                                  "5" "6" "7" "8" "9".

           03  P-POS          PIC 9(6).
           88  P-NONDISP                    VALUE ZEROS.
           88  P-POS-VALID  VALUE 001001 THRU 001078 002001 THRU 002078
                                  003001 THRU 003078 004001 THRU 004078
                                  005001 THRU 005078 006001 THRU 006078
                                  007001 THRU 007078 008001 THRU 008078
                                  009001 THRU 009078 010001 THRU 010078
                                  011001 THRU 011078 012001 THRU 012078
                                  013001 THRU 013078 014001 THRU 014078
                                  015001 THRU 015078 016001 THRU 016078
                                  017001 THRU 017078 018001 THRU 018078
                                  019001 THRU 019078 020001 THRU 020078
                                  021001 THRU 021078 022001 THRU 022078
                                  023001 THRU 023079 000000.
           03  P-POSITION REDEFINES P-POS.
               05  P-LINE     PIC 9(03).
               88  P-LINE-VALID             VALUE 1 THRU 23.
               05  P-COL      PIC 9(03).
               88  P-COL-VALID              VALUE 1 THRU 79.
           03  P-BUF          VALUE SPACES.
               05  PRU          PIC 99.
               88  PRU-VALID              VALUE 00.

      * NEEDED CALLING PROGRAMS DIR AND PROG FOR HELP FILES
           03  P-CALLING-DIR    PIC XX.
           03  P-CALLING-PROG   PIC X(6).

      *=================================================================
      * P-TIMER-LINK-BUF IS AN EXACT COPY IN LIBGB/TIMER_LNK.CPY
      * P-TIMER-LINK-BUF IS AN EXACT COPY IN LIBGB/TIMEWK.CPY
      * ANY CHANGE HERE MUST BE MADE IN THE OTHER COPY MEMBERS!!!!!
      *=================================================================
           03  P-TIMER-TYPE-SET             PIC X(1).
               88  P-TIMER-TYPE-SET-VALID   VALUE "E" "R" "U".

           03  P-TIMER-LINK-BUF.
               05  P-TIMER-STAT             PIC X(02).
               05  P-TIMER-BEG-DATE         PIC 9(08).
               05  P-TIMER-DAYSEC-BEG       PIC 99V9(7).
               05  P-TIMER-DAYSEC-END       PIC 99V9(7).
               05  P-TIMER-DAYSEC-ELAPSED   PIC 99V9(7).
               05  P-TIMER-BEG-EDIT.
                   07  P-TIMER-BEG-HHMMSS   PIC 99/99/99.
                   07  FILLER               PIC X(01).
                   07  P-TIMER-BEG-AMPM     PIC X(02).
               05  P-TIMER-END-EDIT.
                   07  P-TIMER-END-HHMMSS   PIC 99/99/99.
                   07  FILLER               PIC X(01).
                   07  P-TIMER-END-AMPM     PIC X(02).
               05  P-TIMER-ELAPSED-EDIT     PIC 99/99/99/99.
               05  P-TIMER-TYPE             PIC X(1).
                   88  P-TIMER-TYPE-EXCEL   VALUE "E".
                   88  P-TIMER-TYPE-REPORT  VALUE "R".
                   88  P-TIMER-TYPE-UTILITY VALUE "U".
                   88  P-TIMER-TYPE-VALID   VALUE "E" "R" "U".

