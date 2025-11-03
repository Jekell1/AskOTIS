       IDENTIFICATION  DIVISION.
       PROGRAM-ID.        BBINQ.
       DATE-WRITTEN. 11/12/1999.
      ******************************************************************
      *                 (WI)
      *            BULLETIN BOARD INQUIRY
      *
      *       CALLED FROM CL/WIMENU
      *
      * REV:
      * BLV 010417 IF LOCATION IS GROUP INSTEAD OF BR#, READ GRFILE &
      *            DISPLAY GROUP ID & GROUP PHONE INSTEAD OF BRANCH;
      *            WORLD PR# 259
      *  CS 181018 SET-BB-SELECT-PATH 
      * BAH 20191007 REMOVED ACCESS-CALL BBFILE
      ******************************************************************
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
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)WI/BBINQ.CBL S35 04/22/24-10:24:37 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBSP/SP01BB.CPY".
       COPY "LIBSP/SP01BB_SQL.CPY".
       COPY "LIBSP/SPWSBB.CPY".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************

       01  ERRCD             PIC X     VALUE SPACES.
       01  HOLD-LOGNAME      PIC X(10) VALUE SPACES.
       01  ONE               PIC S9.
       01  SUB               PIC 999 COMP   VALUE 0.
       01  IO-TYPE           PIC X(3).

       01  BBINQ-LINES           PIC 99   VALUE 22.
       01  BBINQ-COLUMNS         PIC 99   VALUE 80.
       01  BBINQ-BEGIN-Y         PIC 99   VALUE 1.
       01  BBINQ-BEGIN-X         PIC 99   VALUE 1.

      *-----------------------------------------------------------------
      * 78 LEVEL CONSTANTS
      *-----------------------------------------------------------------
       78  TYPE-HEADER-LIST             VALUE "HEADER:P.".
       78  AM-CMD                       VALUE X"616D".
       78  PM-CMD                       VALUE X"706D".
       78  TYPE-LIST                    VALUE "TYPE_P:P TYPE.".
       78  TYPE2-LIST                   VALUE "TYPE2_P:P.".
       78  MSEL-LIST                    VALUE "MSEL:P.".
       78  FIRST-FIELD                  VALUE "010.".
       78  LAST-FIELD                   VALUE "150.".
       78  MAX-PAGES                    VALUE 600.

      *-----------------------------------------------------------------
      * CHANGE HEADER TO MATCH DETAIL.
      *  IF SORTING BY USERIS, PUT USERID FIRST IN DISPLAY
      *  IF SORTING BY NAME, PUT USER NAME FIRST IN DISPLAY
      *-----------------------------------------------------------------
       01  TYPE-HEADER1.
           03  FILLER        PIC X(40) VALUE
           "LOGIN      NAME                 LOCATION".
           03  FILLER        PIC X(22) VALUE
           "   PHONE        ACTION".
       01  TYPE-HEADER2.
           03  FILLER        PIC X(40) VALUE
           "NAME                  LOGIN     LOCATION".
           03  FILLER        PIC X(22) VALUE
           "   PHONE        ACTION".

      *-----------------------------------------------------------------
      * LEGENDS FOR EACH FIELD ON THE SCREEN
      *-----------------------------------------------------------------
       01  LEGEND            PIC X(60) VALUE SPACES.
       01  LEGEND-LEGEND.
           03  FILLER        PIC X(30)   VALUE
               "          F1, F7-EXIT, F5-EDIT".
           03  FILLER        PIC X(30)   VALUE
               ", F6/ENTER-MORE INFO          ".
       01  LEGEND-USERID.
           03  FILLER        PIC X(30)   VALUE
               "         F7, UP-KEY, ENTER, EN".
           03  FILLER        PIC X(30)   VALUE
               "TER USERID FOR SCAN          ".
       01  LEGEND-NAME.
           03  FILLER        PIC X(30)   VALUE
               "        F7, UP-KEY, ENTER, ENT".
           03  FILLER        PIC X(30)   VALUE
               "ER USER NAME FOR SCAN         ".
       01  LEGEND-TYPE.
           03  FILLER        PIC X(30)   VALUE
               "                 SCAN TYPE: 1-".
           03  FILLER        PIC X(30)   VALUE
               "USERID, 2-NAME".

      *-----------------------------------------------------------------
      * SCAN RANGES          INQ-TYPE: 1=USERID/2=USER NAME
      *-----------------------------------------------------------------
       01  INQUIRY-RANGES.
           03  INQ-TYPE        PIC 9(01).
           03  BEG-NAME        PIC X(20).
           03  BEG-USERX REDEFINES BEG-NAME.
               05  BEG-USER        PIC X(10).
               03  FILLER          PIC X(10).

      *-----------------------------------------------------------------
      * FIELD SELECTION WORKERS
      *-----------------------------------------------------------------
       01  FLDSEL-STAT     PIC XX.
           88  FLDSEL-F1-KEY             VALUE "10".
           88  FLDSEL-F2-KEY             VALUE "12".
           88  FLDSEL-F3-KEY             VALUE "16".
           88  FLDSEL-F4-KEY             VALUE "18".
           88  FLDSEL-F5-KEY             VALUE "1:".
           88  FLDSEL-F6-KEY             VALUE "1<".
           88  FLDSEL-F7-KEY             VALUE "1>".
           88  FLDSEL-F8-KEY             VALUE "1 ".
           88  FLDSEL-F9-KEY             VALUE "1(".
           88  FLDSEL-F10-KEY            VALUE "1)".
           88  FLDSEL-F11-KEY            VALUE "1!".
           88  FLDSEL-F12-KEY            VALUE "1#".
           88  FLDSEL-DOWN-KEY           VALUE "1D".
           88  FLDSEL-ENTER-KEY          VALUE "00".
           88  FLDSEL-HOME-KEY           VALUE "1H".
           88  FLDSEL-LEFT-KEY           VALUE "1L".
           88  FLDSEL-PAGE-UP-KEY        VALUE "1P".
           88  FLDSEL-PAGE-DOWN-KEY      VALUE "1N".
           88  FLDSEL-RIGHT-KEY          VALUE "1R".
           88  FLDSEL-UP-KEY             VALUE "1U".

       01  FLDSEL-LIST.
           03  FILLER      PIC X(40)  VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(20)  VALUE
               "110 120 130 140 150.".
       01  FLDSEL-BUF.
           03  FILLER                  PIC X.
           03  FLDSEL-TYPE             PIC X(31).
           03  FLDSEL-TYPE1 REDEFINES FLDSEL-TYPE.
               05  FLDSEL-LOGIN1.
                   07   FILLER             PIC X.
                   07   FLDSEL-BRNO1       PIC 9(4).
                   07   FILLER             PIC X(4).
               05  FILLER                  PIC X(02).
               05  FLDSEL-CONTACT1         PIC X(20).
           03  FLDSEL-TYPE2 REDEFINES FLDSEL-TYPE.
               05  FLDSEL-CONTACT2         PIC X(20).
               05  FILLER                  PIC X(02).
               05  FLDSEL-LOGIN2.
                   07   FILLER             PIC X.
                   07   FLDSEL-BRNO2       PIC 9(4).
                   07   FILLER             PIC X(4).
           03  FILLER                  PIC X(03).
           03  FLDSEL-BRNO             PIC Z(04).
           03  FILLER                  PIC X(02).
           03  FLDSEL-BRPHONE          PIC X(12).
           03  FILLER                  PIC X(04).
           03  FLDSEL-ACTION           PIC X(19).


       01  FLDSEL-FIELD  PIC X(6)  VALUE "010.  ".
      *       THE ABOVE FIELD IS 2 EXTRA ON PURPOSE

      *-----------------------------------------------------------------
      * DISPLAY BUFFER
      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER      PIC X(40)  VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(35)  VALUE
               "110 120 130 140 150 PGNO ENDMORE.".
       01  DISPLAY-BUF.
           03  D-LINE                 OCCURS 15 TIMES.
               05  FILLER             PIC X.
               05  D-TYPE.
                   07  D-LOGIN1           PIC X(9).
                   07  FILLER             PIC X(02).
                   07  D-CONTACT1         PIC X(20).
               05  D-TYPE2  REDEFINES D-TYPE.
                   07  D-CONTACT2         PIC X(20).
                   07  FILLER             PIC X(02).
                   07  D-LOGIN2           PIC X(9).
               05  FILLER             PIC X(03).
               05  D-BRNO             PIC Z(04).
               05  D-GROUP REDEFINES D-BRNO
                                      PIC X(04).
               05  FILLER             PIC X(02).
               05  D-BRPHONE          PIC X(12).
               05  FILLER             PIC X(04).
               05  D-ACTION           PIC X(19).
           03  D-PAGENO               PIC ZZ9.
           03  D-END-MORE             PIC X(7).

       01  PAGE-KEY-TABLE.
      *    03  PAGE-KEY               PIC X(28) OCCURS 600.
           03  PAGE-KEY                         OCCURS 600.
               05  PAGE-KEY-CAT       PIC X(04).
               05  PAGE-KEY-SUB       PIC X(04).
               05  PAGE-KEY-NAM       PIC X(20).

       01  CURRENT-PAGE               PIC S999.
       01  PAGE-COUNT                 PIC 999.

      *--------------------------------------------
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  ADMIN-SEQ                    VALUE 0.
       78  VIEW-SEQ                     VALUE 1.
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBWI/BBINQ2W.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       01  BBINQ-WINDOW-HANDLE    PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/BBINQ_DEF.CPY".
       COPY "LIBWI/BBINQ_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/BBINQ_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      * MAIN PROGRAM MODULE
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "BBINQ" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE BBINQ-BEGIN-Y * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  BBINQ-BEGIN-X * VDU-WIN-C-OFFSET
                   SIZE    BBINQ-COLUMNS + VDU-WIN-W-MOD
                   LINES   BBINQ-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS BBINQ-WINDOW-HANDLE.

           PERFORM FORM-RESET.


      * DEFAULT SORT ORDER IS BY NAME

           MOVE 2 TO INQ-TYPE.

       MAIN-CONT.
           PERFORM REC-INQ-RANGES.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-CONT.

           PERFORM INQUIRY-DISPLAY.

           GO TO MAIN-CONT.

      ******************************************************************
       REC-INQ-RANGES SECTION.
      ******************************************************************
           MOVE SPACES TO IO-TYPE.

           GO TO USER-01.

       TYPE-01.
           MOVE "TYPE: " TO WR-BUF.
           MOVE TYPE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE LEGEND-TYPE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "EN N010TYPE." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
                     MOVE "END" TO IO-TYPE
                     GO TO END-REC-RANGES.
           IF NOT (ENTER-KEY OR DOWN-KEY ) GO TO USER-01.
           MOVE VDUNUM0 TO INQ-TYPE.
           IF NOT (INQ-TYPE = 1 OR 2)
              GO TO TYPE-01.

           MOVE SPACES TO WR-BUF.
           MOVE TYPE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       USER-01.
           EVALUATE INQ-TYPE
             WHEN 1 MOVE LEGEND-USERID TO LEGEND
                    MOVE "USERID:" TO WR-BUF
                    MOVE TYPE2-LIST TO WR-LIST
                    PERFORM CALL-WRFORM
                    MOVE TYPE-HEADER1 TO WR-BUF
             WHEN 2 MOVE LEGEND-NAME TO LEGEND
                    MOVE "  NAME:" TO WR-BUF
                    MOVE TYPE2-LIST TO WR-LIST
                    PERFORM CALL-WRFORM
                    MOVE TYPE-HEADER2 TO WR-BUF
      *             CALL XFER USING STAT TYPE-HEADER-LIST TYPE-HEADER2
           END-EVALUATE.
      *    CALL XFER USING STAT TYPE-HEADER-LIST.
           MOVE TYPE-HEADER-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           PERFORM SEND-LEGEND.


      *    -------------------------------------------------------------
      *    DISPLAY DASHES SO THAT USER KNOWS HOW BIG A FIELD
      *    THEY ARE TO INPUT (WE SCAN IN 20 EVEN IF THEY
      *    ARE INPUTTING A USERID (10 CHARS))
      *    -------------------------------------------------------------
           MOVE "S  A020USR1." TO VDU.
           IF INQ-TYPE = 1
              MOVE "__________" TO VDUBUF
           ELSE
              MOVE "____________________" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A020USR1." TO VDU.
           PERFORM SCREENIO.
           IF F4-KEY
                     MOVE ADMIN-SEQ TO PASSWDW-SEQ
                     MOVE "TO CREATE/EDIT RECORDS" TO PASSWDW-PROMPT2
                     PERFORM VERIFY-PASSWORD
                     IF PASSWDW-STAT = "OK"
                        MOVE "N" TO BBINQ2-ACTION
                        PERFORM MORE-INFO
                        GO TO USER-01
                     END-IF
                     GO TO USER-01.
           IF F3-KEY GO TO ALL-USERS.
           IF F7-KEY
                     MOVE "END" TO IO-TYPE
                     GO TO END-REC-RANGES.
           IF UP-KEY GO TO TYPE-01.
           IF NOT (ENTER-KEY OR DOWN-KEY OR F6-KEY ) GO TO USER-01.

      *    -------------------------------------------------------------
      *    IF INPUTTING USERID CONVERT TO LOWER AND REDISPLAY
      *    I DO THIS BECAUSE I ALLOW THEM TO ENTER IN UPPER CASE
      *    -------------------------------------------------------------
           IF INQ-TYPE = 1
              MOVE VDUBUF TO BEG-USER
              CALL "C$TOLOWER" USING BEG-USER, VALUE 10
              MOVE "S  A010USR1." TO VDU
              MOVE BEG-USER TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE VDUBUF TO BEG-USER.

           GO TO END-REC-RANGES.
       ALL-USERS.
           MOVE "S  A010USR1." TO VDU.
           MOVE SPACES TO VDUBUF BEG-USER.
           PERFORM SCREENIO.

       END-REC-RANGES.
           EXIT.

      ******************************************************************
       INQUIRY-DISPLAY SECTION.
      ******************************************************************
           MOVE SPACES TO BB1-KEY.
           MOVE 0      TO BB-SEQNO.

           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           EVALUATE INQ-TYPE
              WHEN 1 PERFORM OPEN-BB1-FILE
                     MOVE BB1-KEY TO PAGE-KEY(PAGE-COUNT)
              WHEN 2 PERFORM OPEN-BB2-FILE
                     MOVE BB-CATAGORY    TO PAGE-KEY-CAT(PAGE-COUNT)
                     MOVE BB-SUBCATAGORY TO PAGE-KEY-SUB(PAGE-COUNT)
                     MOVE BB-NAME        TO PAGE-KEY-NAM(PAGE-COUNT)
           END-EVALUATE.

       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.
           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.
           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

       CONT-BUILD-PAGENO.
           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1 TO SUB.
           EVALUATE INQ-TYPE
              WHEN 1 MOVE PAGE-KEY(CURRENT-PAGE) TO BB1-KEY
                     PERFORM START-BB1-FILE
              WHEN 2 MOVE PAGE-KEY-CAT(CURRENT-PAGE)  TO BB-CATAGORY
                     MOVE PAGE-KEY-SUB(CURRENT-PAGE)  TO BB-SUBCATAGORY
                     MOVE PAGE-KEY-NAM(CURRENT-PAGE)  TO BB-NAME
                     PERFORM START-BB2-FILE
           END-EVALUATE.

      ******************************************************************
      * BUILD PAGE
      ******************************************************************
       BUILD-PAGENO.
           EVALUATE INQ-TYPE
              WHEN 1 PERFORM READ-BB1-FILE-NEXT
              WHEN 2 PERFORM READ-BB2-FILE-NEXT
           END-EVALUATE
           IF IO-FG NOT = 0
              GO TO DISPLAY-PAGE.

           IF BB-CATAGORY NOT = "MGR"
              GO TO BUILD-PAGENO.

           IF BB-SUBCATAGORY NOT = "GRP"
              GO TO BUILD-PAGENO.

           IF INQ-TYPE = 1
              IF BB-USERID < BEG-USER
                 GO TO BUILD-PAGENO
              END-IF
           ELSE
              IF BB-NAME < BEG-NAME
                 GO TO BUILD-PAGENO.


           IF SUB > 15
              GO TO DISPLAY-PAGE.
      *-----------------------------------------------------------------

       MOVE-FIELDS.
           EVALUATE INQ-TYPE
              WHEN 1  MOVE BB-USERID TO D-LOGIN1(SUB)
                      MOVE BB-NAME   TO D-CONTACT1(SUB)
              WHEN 2  MOVE BB-USERID TO D-LOGIN2(SUB)
                      MOVE BB-NAME   TO D-CONTACT2(SUB)
           END-EVALUATE.
           IF BB-BRNO = 0
              MOVE BB-GROUP TO D-GROUP(SUB) GR-ID
              PERFORM GET-GR
              IF IO-BAD
                 MOVE SPACES     TO D-BRPHONE(SUB)
              END-IF
              MOVE GR-PHONE   TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT   TO D-BRPHONE(SUB)
           ELSE
              MOVE BB-BRNO    TO D-BRNO(SUB) BR-NO
              PERFORM GET-BR
              MOVE BR-PHONE   TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT   TO D-BRPHONE(SUB).
           MOVE BB-ACTION  TO D-ACTION(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      ******************************************************************
      *    DISPLAY PAGE
      ******************************************************************
       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-MESS
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO END-INQUIRY-MODULE.
           MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    EVALUATE INQ-TYPE
                       WHEN 1 MOVE BB1-KEY TO PAGE-KEY(PAGE-COUNT)
                       WHEN 2 MOVE BB-CATAGORY
                                           TO PAGE-KEY-CAT(PAGE-COUNT)
                              MOVE BB-SUBCATAGORY
                                           TO PAGE-KEY-SUB(PAGE-COUNT)
                              MOVE BB-NAME TO PAGE-KEY-NAM(PAGE-COUNT)
                    END-EVALUATE
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO LEGEND
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.
           MOVE CURRENT-PAGE TO D-PAGENO.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
      * SELECT FIELD FROM WINDOW
      ******************************************************************
       SELECT-FIELD-FROM-DISPLAY.
           MOVE LEGEND-LEGEND TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.

           IF FLDSEL-PAGE-UP-KEY OR FLDSEL-UP-KEY
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

           IF FLDSEL-PAGE-DOWN-KEY OR FLDSEL-DOWN-KEY
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

           IF FLDSEL-HOME-KEY
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

           IF FLDSEL-F1-KEY
              GO TO END-INQUIRY-MODULE.

           IF FLDSEL-F3-KEY
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

           IF FLDSEL-F4-KEY
              MOVE ADMIN-SEQ TO PASSWDW-SEQ
              MOVE "TO EDIT POSTINGS" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT = "OK"
                 MOVE "A" TO BBINQ2-ACTION
                 PERFORM MORE-INFO
                 MOVE 0 TO ONE
                 GO TO START-BUILD-PAGENO
              END-IF
              GO TO SELECT-FIELD-FROM-DISPLAY.

      *    -------------------------------------------------------------
      *    I WANT TO MOVE MOVE 0 TO "ONE" HER TO FORCE REDISPLAY OF
      *    PAGE TO REFLECT CHANGES MADE TO FIELDS DISPLAYED
      *    -------------------------------------------------------------
           IF FLDSEL-F5-KEY
              MOVE "O" TO BBINQ2-ACTION
              PERFORM MORE-INFO
              MOVE 0 TO ONE
              GO TO START-BUILD-PAGENO.

           IF FLDSEL-F7-KEY
              GO TO END-ROUTINE.

      *    -------------------------------------------------------------
      *    I WANT TO MOVE MOVE 0 TO "ONE" HERE TO FORCE REDISPLAY OF
      *    PAGE TO REFLECT CHANGES MADE TO FIELDS DISPLAYED
      *    -------------------------------------------------------------
           IF FLDSEL-ENTER-KEY OR FLDSEL-F6-KEY
              MOVE VIEW-SEQ TO PASSWDW-SEQ
              MOVE "TO VIEW MORE INFORMATION" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO SELECT-FIELD-FROM-DISPLAY
              END-IF
              ACCEPT HOLD-LOGNAME FROM ENVIRONMENT "LOGNAME"
              EVALUATE INQ-TYPE
                 WHEN 1 IF HOLD-LOGNAME = FLDSEL-LOGIN1
                           MOVE "O" TO BBINQ2-ACTION
                        ELSE
                           MOVE "V" TO BBINQ2-ACTION
                        END-IF
                 WHEN 2 IF HOLD-LOGNAME = FLDSEL-LOGIN2
                           MOVE "O" TO BBINQ2-ACTION
                        ELSE
                           MOVE "V" TO BBINQ2-ACTION
                        END-IF
              END-EVALUATE
              PERFORM MORE-INFO
              MOVE 0 TO ONE
              GO TO START-BUILD-PAGENO.

           IF NOT FLDSEL-ENTER-KEY
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           GO TO SELECT-FIELD-FROM-DISPLAY.

       END-INQUIRY-MODULE.
           EXIT.

      ******************************************************************
      * MORE INFORMATION WINDOW
      *
      * DESC: DISPLAYS MORE DETAILED INFORMATION ON THE USER.
      *
      *  BBINQ2-ACTION : "O" - EDIT OLD RECORD (EDIT MODE)
      *                  "V" - VIEW RECORD     (VIEW MODE)
      *                  "N" - NEW RECORD      (ADMIN MODE)
      *                  "A" - EDIT RECORD     (ADMIN MODE)
      *
      * NOTE: SOME INFORMATION IS REDUNDANT BUT IS DISPLAYED BECAUSE
      *       THE MORE INFORMATION WINDOW COVERS UP THE SCAN WINDOW.
      ******************************************************************
       MORE-INFO SECTION.
           MOVE "MGR"  TO BBINQ2-CATAGORY.
           MOVE "GRP " TO BBINQ2-SUBCATAGORY.

           IF BBINQ2-ACTION = "N"
              MOVE SPACES         TO BBINQ2-LOGIN
                                     BBINQ2-USERID
              MOVE ZEROS          TO BBINQ2-SEQNO
           ELSE
              EVALUATE INQ-TYPE
                 WHEN 1 MOVE FLDSEL-LOGIN1   TO BBINQ2-LOGIN
                                                BBINQ2-USERID
                 WHEN 2 MOVE FLDSEL-LOGIN2   TO BBINQ2-LOGIN
                                                BBINQ2-USERID
              END-EVALUATE
              MOVE 000001         TO BBINQ2-SEQNO.

           MOVE "WI/BBINQ2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BBINQ2-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "BBINQ" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ******************************************************************
       GET-BR SECTION.
      ******************************************************************
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO BR-NAME  BR-ADR1  BR-ADR2
                             BR-CITY  BR-STATE BR-ZIP
              MOVE 0 TO BR-NO BR-PHONE.

      ******************************************************************
       GET-GR SECTION.
      ******************************************************************
           PERFORM OPEN-GR1-FILE.
           PERFORM READ-GR1-FILE.
           PERFORM CLOSE-GR1-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO GR-NAME
              MOVE 0 TO GR-PHONE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY BBINQ-SCREEN UPON BBINQ-WINDOW-HANDLE.
           MOVE BBINQ-FORM-FIELDS TO WR-BUF.
           MOVE BBINQ-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE BBINQ-HELP--FILE TO HELP-LINK-FILE.
           MOVE BBINQ-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/BBINQ_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BB2-FILE.
           PERFORM CLOSE-BB1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBSP/SPBBGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBSP/SPBB1RN.CPY".
       COPY "LIBSP/SPBB2RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BBINQ-SCREEN.
           DESTROY BBINQ-WINDOW-HANDLE.
           MOVE FORM-PATHNAME TO FORM-PATH.
       STOP-RUN.
           EXIT PROGRAM.
