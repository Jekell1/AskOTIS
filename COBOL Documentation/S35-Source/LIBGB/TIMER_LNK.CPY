      * COPYMEMBER: LIBGB/TIMER_LNK
      ******************************************************************
      * ****NOTE**** ****NOTE**** ****NOTE**** ****NOTE**** ****NOTE****
      *
      * TIMER-LINK-BUF IS AN EXACT COPY IN LIBGB/PRU_LNK.CPY
      * TIMER-LINK-BUF IS AN EXACT COPY IN LIBGB/TIMERWK.CPY
      * ANY CHANGE HERE MUST BE MADE IN THE OTHER COPY MEMBERS!!!!!
      *
      * ****NOTE**** ****NOTE**** ****NOTE**** ****NOTE**** ****NOTE****
      ******************************************************************
      *
      * IDENTIFICATION: LIBGB/TIMER_LNK.CPY
      *
      * DESCRIPTION   : TIMER UTILITY LINKAGE WORKERS
      *
      *=================================================================
      * LINKAGE USES TIMER-TYPE WHICH IT SETTINGS DOES:
      *
      * "E" = EXCEL   | DISPLAY RESULTS WITH MS EXCEL FORMAT
      *               | AND RETURN RESULTS.
      *
      * "R" = REPORT  | DISPLAY RESULTS WITHOUT MS EXCEL FORMAT
      *               | AND RETURN RESULTS.
      *
      * "U" = UTILITY | RETURNS RESULTS; NO DISPLAY
      *
      *=================================================================
      * TIME FORMAT OF HH:MM:SS CAN BE CONVERTED TO MS EXCEL TIME
      * FORMAT WHICH IS BASED ON A CALCULATION OF 86,400 SECONDS IN
      * A DAY.  THE CONVERSION CAN BE REVERSED BACK TO HH:MM:SS FORMAT.
      *
      * BY USING A BEGINNING AND ENDING TIMES IN MS EXCEL FORMAT YOU CAN
      * CALCUATE THE ELAPSED TIME.
      *
      *=================================================================
      * REV:
      * JKC 2020-1026 ***NEW***  USED FOR GB/TIMER.CBL
      *
      * SMR 2024-0401 ADDED TIMER-BEG-DATE TO BE USED FOR CALCULATING
      *          THE ELAPSED TIME IF THE END TIME CROSS OVER TO A NEW
      *          DAY.
      *
      ******************************************************************
       01  TIMER-LINK-BUF.
           03  TIMER-STAT                  PIC X(02).
           03  TIMER-BEG-DATE              PIC 9(08).
           03  TIMER-DAYSEC-BEG            PIC 99V9(7).
           03  TIMER-DAYSEC-END            PIC 99V9(7).
           03  TIMER-DAYSEC-ELAPSED        PIC 99V9(7).
           03  TIMER-BEG-EDIT.
               05  TIMER-BEG-HHMMSS        PIC 99/99/99.
               05  FILLER                  PIC X(01).
               05  TIMER-BEG-AMPM          PIC X(02).
           03  TIMER-END-EDIT.
               05  TIMER-END-HHMMSS        PIC 99/99/99.
               05  FILLER                  PIC X(01).
               05  TIMER-END-AMPM          PIC X(02).
           03  TIMER-ELAPSED-EDIT          PIC 99/99/99/99.
           03  TIMER-TYPE                  PIC X(1).
               88  TIMER-TYPE-EXCEL        VALUE "E".
               88  TIMER-TYPE-REPORT       VALUE "R".
               88  TIMER-TYPE-UTILITY      VALUE "U".
               88  TIMER-TYPE-VALID        VALUE "E" "R" "U".

