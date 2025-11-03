       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CLSCAN.
      **************************************************************************
      *  CAN-LOAN / LOANS FILE WINDOW SCAN
      *
      * CALLED FROM:  WI/CLQUOT (F2-SCAN)
      *
      * REV:
      *  JTG 032798 MAKE WINDOW DO AN INITIAL DISPLAY WHEN CALLED
      *  JTG 040212 ALLOWED NON MONTHLY CAN LOANS (NMQUOT.C) WORLD J043
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./WI/CLSCAN.CBL S35 09/20/22-14:08:51 >".
       COPY "LIBLP/LP01CL.CPY".
       COPY "LIBLP/LP01CL_SQL.CPY".
       COPY "LIBLP/LPWSCL.CPY".

       01  CLSCAN-LINES              PIC 99   VALUE 18.
       01  CLSCAN-COLUMNS            PIC 99   VALUE 41.
       01  CLSCAN-BEGIN-Y            PIC 99   VALUE 6.
       01  CLSCAN-BEGIN-X            PIC 99   VALUE 2.
       01  ERRCD                    PIC X      VALUE SPACES.
       01  ONE                      PIC S9     VALUE 1.
       01  SUB                      PIC 99.
       01  SCAN-TYPE                PIC 9      VALUE 2.

       01  FLDSEL-STAT              PIC XX.
       01  FLDSEL-FIELD             PIC X(4)   VALUE "010.".
       01  FLDSEL-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           03  FLDSEL-1.
               05  FLDSEL-1-CAN-NO   PIC X(2).
               05  FILLER            PIC XX.
               05  FLDSEL-1-DESC     PIC X(33).
           03  FLDSEL-2             REDEFINES FLDSEL-1.
               05  FLDSEL-2-DESC     PIC X(33).
               05  FILLER            PIC X(2).
               05  FLDSEL-2-CAN-NO   PIC X(2).

       01  FIRST-FIELD               PIC X(4)  VALUE "010.".
       01  LAST-FIELD                PIC X(4)  VALUE "090.".


       01  DISPLAY-LIST.
           03  FILLER                PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE                OCCURS 9 TIMES.
               05  D-1.
                   07  D-1-CAN-NO    PIC X(2).
                   07  FILLER        PIC XX.
                   07  D-1-DESC      PIC X(33).
               05  D-2              REDEFINES D-1.
                   07  D-2-DESC      PIC X(33).
                   07  FILLER        PIC XX.
                   07  D-2-CAN-NO    PIC X(2).
           03  D-PAGENO              PIC ZZ9.
           03  D-END-MORE            PIC X(7).

       01  CURRENT-PAGE              PIC S999.
       01  PAGE-COUNT                PIC 999.
       01  MAX-PAGES                 PIC 999    VALUE 50.
       01  PAGEKEY.
           03  FILLER                    OCCURS 50 TIMES.
               05  PAGEKEY-1.
                   07  PAGEKEY-1-CAN-NO    PIC X(2).
                   07  PAGEKEY-1-DESC      PIC X(33).
               05  PAGEKEY-2            REDEFINES PAGEKEY-1.
                   07  PAGEKEY-2-DESC    PIC X(33).
                   07  PAGEKEY-2-CAN-NO  PIC X(2).

       COPY "LIBLP/LP01PD.CPY".

       COPY "LIBWI/CLSCAN_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CLSCAN_DEF.CPY".
       COPY "LIBWI/CLSCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  CLSCAN-WINDOW-HANDLE    PIC X(10).

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/CLSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CLSCAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME CLSCAN-BUF EXIT-PATHNAME.

      ****************************
       MAIN-PROGRAM SECTION.
      ****************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE " "  TO VDU-WIMENU-SELECTION.
           MOVE "XX" TO CLSCAN-STATUS.

           MOVE "CLSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT SCREEN LINE CLSCAN-BEGIN-X 
                   AT SCREEN COL  CLSCAN-BEGIN-Y
                     CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   SIZE    CLSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   CLSCAN-LINES + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE  IS CLSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE "S  A040GROUPID." TO VDU.
           MOVE CLSCAN-GROUP-ID TO VDUBUF.
           PERFORM SCREENIO.

           IF CLSCAN-SCAN-TYPE NOT = 0
              MOVE CLSCAN-SCAN-TYPE TO SCAN-TYPE.

           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           PERFORM OPEN-CAN-LOAN.
           IF SCAN-TYPE = 1
              MOVE " " TO PAGEKEY-1-CAN-NO(PAGE-COUNT)
                          PAGEKEY-1-DESC(PAGE-COUNT)
           ELSE
           IF SCAN-TYPE = 2
              MOVE " " TO PAGEKEY-2-DESC(PAGE-COUNT)
                          PAGEKEY-2-CAN-NO(PAGE-COUNT).
           PERFORM SEND-PROMPT.
           GO TO START-BUILD-PAGENO.


       ENTER-SCANKEY.
           PERFORM SEND-PROMPT.
           IF SCAN-TYPE = 1
              MOVE LEGEND-1 TO MESS
           ELSE
           IF SCAN-TYPE = 2
              MOVE LEGEND-2 TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E RA330SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              IF SCAN-TYPE = 1
                 MOVE 2 TO SCAN-TYPE
              ELSE
                 MOVE 1 TO SCAN-TYPE
              END-IF
              PERFORM ALARM
              GO TO ENTER-SCANKEY.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SCANKEY.

       ENTER-SCANKEY-CONT.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           PERFORM OPEN-CAN-LOAN.
           IF SCAN-TYPE = 1
              MOVE VDUBUF TO PAGEKEY-1-CAN-NO(PAGE-COUNT)
              MOVE " " TO PAGEKEY-1-DESC(PAGE-COUNT)
           ELSE
           IF SCAN-TYPE = 2
              MOVE VDUBUF TO PAGEKEY-2-DESC(PAGE-COUNT)
              MOVE " " TO PAGEKEY-2-CAN-NO(PAGE-COUNT).

       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.
           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.
           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1 TO SUB.
      * NOTE: CL-CL-GROUP-ID & CL-GROUP-LOC ARE THE SAME FIELD:
           MOVE CLSCAN-GROUP-ID TO CL-GROUP-ID.
           IF SCAN-TYPE = 1
              MOVE PAGEKEY-1-CAN-NO(CURRENT-PAGE) TO CL-CAN-NO
           ELSE
           IF SCAN-TYPE = 2
              MOVE PAGEKEY-2-DESC(CURRENT-PAGE) TO CL-DESC.
           PERFORM START-CAN-LOAN.

       BUILD-PAGENO.
           PERFORM READ-CAN-LOAN-NEXT.
           IF IO-FG NOT = 0
              GO TO DISPLAY-PAGE.
      * NOTE: CL-CL-GROUP-ID & CL-GROUP-LOC ARE THE SAME FIELD:
           IF CL-GROUP-ID NOT = CLSCAN-GROUP-ID
              MOVE 9 TO IO-FG
              GO TO DISPLAY-PAGE.

      *-------------------------------------------------------------
      *    ALL CL-RECORDS THAT ARE PRODUCED FROM 'WI/NMQUOT.C'
      *        VIA CALCZ3.C CONTAIN AN AMOUNT IN PD-LASTPYAMT,
      *        EXCEPT;
      *                SINGLE PAYS WHICH HAVE LASTPYAMT = ZERO.
      *                NOTE: SINGLE PAYMENT LOANS CAN ONLY BE BOOKED
      *                      VIA CALCZ3.C
      *    ALL CL-RECORDS THAT ARE PRODUCED FROM 'WI/CLQUOT.C'
      *        VIA CALCZL.C CONTAIN ZERO IN PD-LASTPYAMT,
      *        EXCEPT;
      *               SINGLE PAYS WHICH HAVE LASTPYAMT = ZERO.
      *               NOTE: SINGLE PAYMENT LOANS CAN ONLY BE BOOKED
      *                     VIA CALCZ3.C
      *
           MOVE CL-PD-REC TO PD-REC.
      * CLSCAN-NON-MONTHLY = 'A' MEANS DISPLAY ALL CAN LOANS
           IF CLSCAN-NON-MONTHLY NOT = "A"
              IF CLSCAN-NON-MONTHLY = "Y"
                 IF ( PD-ORGTERM = 1 AND PD-NOREGPY = 1   ) OR
                    ( PD-ORGTERM > 1 AND PD-LASTPYAMT = 0 )
                    GO TO BUILD-PAGENO
                 END-IF
              ELSE
                 IF ( PD-ORGTERM = 1 ) OR ( PD-LASTPYAMT NOT = 0 )
                    GO TO BUILD-PAGENO.
      *-------------------------------------------------------------

           IF SUB > 9
              GO TO DISPLAY-PAGE.

           IF SCAN-TYPE = 1
              MOVE CL-CAN-NO TO D-1-CAN-NO(SUB)
              MOVE CL-DESC TO D-1-DESC(SUB)
           ELSE
           IF SCAN-TYPE = 2
              MOVE CL-DESC TO D-2-DESC(SUB)
              MOVE CL-CAN-NO TO D-2-CAN-NO(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      *************************************
      *    DISPLAY PAGE
      *************************************
       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-LEGEND
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO ENTER-SCANKEY.
           MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    IF SCAN-TYPE = 1
                       MOVE CL-CAN-NO TO PAGEKEY-1-CAN-NO(PAGE-COUNT)
                       MOVE " " TO PAGEKEY-1-DESC(PAGE-COUNT)
                    ELSE
                    IF SCAN-TYPE = 2
                       MOVE CL-DESC TO PAGEKEY-2-DESC(PAGE-COUNT)
                       MOVE CL-CAN-NO TO PAGEKEY-2-CAN-NO(PAGE-COUNT)
                    END-IF
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.
           MOVE CURRENT-PAGE TO D-PAGENO.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *************************************
      *    SELECT FIELD FROM PAGES
      *************************************
       SELECT-FIELD-FROM-DISPLAY.
           MOVE LEGEND-04 TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM CALL-FLDSEL.

      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST HOME:
           IF FLDSEL-STAT = "1H"
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

           IF FLDSEL-STAT = "1>"
              GO TO END-ROUTINE.
           IF FLDSEL-STAT = "10"
              GO TO ENTER-SCANKEY.
           IF NOT (FLDSEL-STAT = "00" OR "1<")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF SCAN-TYPE = 1
              MOVE FLDSEL-1-CAN-NO TO CLSCAN-CAN-NO
              MOVE "00"            TO CLSCAN-STATUS
           ELSE
           IF SCAN-TYPE = 2
              MOVE "00"            TO CLSCAN-STATUS
              MOVE FLDSEL-2-CAN-NO TO CLSCAN-CAN-NO.

           GO TO END-ROUTINE.


      ***************************************
       CAN-LOAN-IO-ROUTINES SECTION.
      ***************************************
       OPEN-CAN-LOAN.
           PERFORM OPEN-CL1-FILE.
       START-CAN-LOAN.
           IF SCAN-TYPE = 1
              PERFORM START-CL1-FILE
           ELSE
           IF SCAN-TYPE = 2
              PERFORM START-CL2-FILE.
       READ-CAN-LOAN-NEXT.
           IF SCAN-TYPE = 1
              PERFORM READ-CL1-FILE-NEXT
           ELSE
           IF SCAN-TYPE = 2
              PERFORM READ-CL2-FILE-NEXT
              IF ( IO-FG = 0            ) AND
                 ( SUB   = 1            ) AND
                 ( CURRENT-PAGE NOT = 1 )
                 PERFORM READ-CL2-FILE-NEXT
                    UNTIL ( CL-DESC = PAGEKEY-2-DESC(CURRENT-PAGE) OR
                            IO-FG = 9
                          ).
       CLOSE-CAN-LOAN.
           PERFORM CLOSE-CL1-FILE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY CLSCAN-SCREEN UPON CLSCAN-WINDOW-HANDLE.
           MOVE CLSCAN-FORM-FIELDS TO WR-BUF.
           MOVE CLSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE CLSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CLSCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CLSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************
       MISC-PARAGRAPHS SECTION.
      **************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       SEND-PROMPT.
           MOVE "S  A340000." TO VDU.
           IF SCAN-TYPE = 1
              MOVE CAN-PROMPT TO VDUBUF
              MOVE LEGEND-1 TO MESS
           ELSE
           IF SCAN-TYPE = 2
              MOVE DESC-PROMPT TO VDUBUF
              MOVE LEGEND-2 TO MESS.
           PERFORM SCREENIO.

      **************************
       IO-ROUTINES SECTION.
      **************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CL2-FILE.
           PERFORM CLOSE-CL1-FILE.
       COPY "LIBLP/LPCLGS_SQL.CPY".
       COPY "LIBLP/LPCL1RN.CPY".
       COPY "LIBLP/LPCL2RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      **************************
       END-ROUTINE SECTION.
      **************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CLSCAN-SCREEN.
           DESTROY CLSCAN-WINDOW-HANDLE.
           EXIT PROGRAM.

