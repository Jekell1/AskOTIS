      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PWDALY.
       DATE-WRITTEN.    042897.
      **************************************************************************
      *  DESC:  DISPLAY DAILY PASSWORD
      *
      *         F12, DOWN ARROW, DOWN ARROW, DOWN ARROW, F12
      *         
      *         CALLED FROM SCREEN.CPY
      * REV:
      *  JTG 110797 STOPPED THIS PROGRAM FROM EXECUTION IF NOT AN 'A' LOGIN
      *  MG  990929 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  MJD 131105 MADE CHANGES FOR A30. CLEANED UP UNUSED FLDSEL ETC
      *             THAT WAS  A CARRY OVER FROM THE PROGRAM USED AS THE
      *             BASE FOR THIS ONE AND NOT USED.
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)CL/PWDALY A30 11/05/13-11:31:20 09/18/14-13:47:06 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       01  ROUTE-BUF               PIC XX  VALUE " ".
       01  EXT-EOBUF-MESS              REDEFINES ROUTE-BUF
                                   PIC XX.
       01  D-DATE1-YYMMDD          PIC X(10)     VALUE " ".
       01  WS-DATE          REDEFINES D-DATE1-YYMMDD.
           03  WS-D1         PIC 9.
           03  WS-D2         PIC 9.
           03  WS-D3         PIC 9.
           03  WS-D4         PIC 9.
           03  WS-D5         PIC 9.
           03  WS-D6         PIC 9.
       01  WS-SUM            PIC 99.
       01  WS-PROD           PIC 9(6).
       01  WS-ANSWER        REDEFINES WS-PROD.
           03  WS-ANS1       PIC 99.
           03  WS-ANS2       PIC 9999.
           03  WS-ANS2-X    REDEFINES WS-ANS2
                             PIC X(4).
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/GETFMW.CPY".
       01  PGMENU-SELECTION    PIC XX.

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME PGMENU-SELECTION.

      /*************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.

           IF NOT ( EXT-CONAME-USER-PROFILE-CD = X"61" )
              GO TO END-ROUTINE.

           ACCEPT D-DATE1-YYMMDD FROM DATE.
           COMPUTE WS-SUM = WS-D1 +
                               WS-D2 +
                                  WS-D3 +
                                     WS-D4 +
                                        WS-D5 +
                                           WS-D6.
           COMPUTE WS-PROD = WS-SUM * WS-SUM * WS-SUM.
           MOVE " " TO MESS.
           STRING "  TODAYS PASSWORD IS: " WS-ANS2-X
               DELIMITED BY SIZE INTO MESS.
           PERFORM SEND-MESS.
           GO TO END-ROUTINE.

       MISC SECTION.
       COPY "LIBGB/SENDMESS.CPY".
       ALARM.
           DISPLAY OMITTED WITH BEEP.
       IO-ERROR SECTION.
           MOVE "WINDOW ERROR " TO MESS.
           PERFORM SEND-MESS.
       END-ROUTINE.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
