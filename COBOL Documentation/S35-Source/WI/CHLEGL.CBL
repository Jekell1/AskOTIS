       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CHLEGL.
       DATE-WRITTEN.  02/22/2011.
      *****************************************************
      *                 (WI)
      *
      *  DESCRIPTION:   LEGAL TRAILER INFO
      *                   CALLED FROM CLMENU (F9)
      *                      (LP/CHLEG2 IS IN LOAN PROC CHANGES MENU)
      *
      *  REV:
      *   BAH 110222 NEW FOR UNITED ACCEPTANCE, PR# 3707
      *   BAH 110804 ADDED 17 AND 18 ON PAGE 2, UNITED PR# 3847
      *   BAH 160209 SPLIT OUT LSFILE, PD0003
      *   BAH 170412 ACTIVATE 8 DIGIT DATES, PD0006
      *   BAH 20240417 REMOVED CHLEGLW, CALL PROGRAM INSTEAD OF WINDOW, NOTHING
      *                PASSED
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/CHLEGL S35 03/07/23-12:21:32 >".
       COPY "LIBLP/LP01LSL.CPY".
       COPY "LIBLP/LP01LSL_SQL.CPY".
       COPY "LIBLP/LPWSLSL.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
      *-----------------------------------------------------------------
       01  LEGEND          PIC X(70).

       01  CHLEGL-LINES              PIC 99  VALUE 22.
       01  CHLEGL-COLUMNS            PIC 99  VALUE 78.
       01  CHLEGL-BEGIN-Y            PIC 99  VALUE 2.
       01  CHLEGL-BEGIN-X            PIC 99  VALUE 2.
       01  ERRCD           PIC X     VALUE SPACES.

       01  SAVE-PATH       PIC X(22).

       01  CHAN              PIC X(4)  VALUE "CHAN".
       01  ONE               PIC S9.
       01  ELE               PIC 99    VALUE 0.
       01  ACTION.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
       01  IO-TYPE           PIC X(3).

      *----------------------------------------------------------------
      * NEW-REC      : "Y" = NEW-RECORD.
      *                "N" = EXISTING RECORD (DEFAULT).
      *
      * FILE-CREATED : "Y" FILE HAS BEEN WRITTEN (DEFAULT).
      *                "N" FILE HAS NOT BEEN WRITTEN (NEW RECORD).
      *----------------------------------------------------------------
       01  NEW-REC           PIC X     VALUE "N".
       01  FILE-CREATED      PIC X     VALUE "Y".

       01  NEW-FLAG          PIC 9.
       01  FIRST-TIME        PIC 9     VALUE 0.

       01  HOLD-LSL1-KEY.
           03  HOLD-BRNO     PIC 9(4).
           03  HOLD-ACCTNO   PIC 9(8).
           03  HOLD-SEQNO    PIC 99.

      * ORIGINAL RECORD
           COPY "LIBLP/LP01LSL.CPY" REPLACING LEADING "LSL" BY "LSL-OR".
      * CHANGED RECORD
           COPY "LIBLP/LP01LSL.CPY" REPLACING LEADING "LSL" BY "LSL-CH".

      *-----------------------------------------------------------------

       01  FORM-BUF.
           03  FILLER        PIC X(9) VALUE "WI/CHLEGL".
           03  FILLER        PIC X(9) VALUE "WI/CHLEG2".
       01  FORM-BUF-RED REDEFINES FORM-BUF.
           03  FORM-NAME     PIC X(9) OCCURS 2.
       01  FORM-BUFX REDEFINES FORM-BUF.
           03  FORM-NAMEX     OCCURS 2.
               05  FILLER    PIC XXX.
               05  FORM-VDU-NAME PIC X(6).


       01  FORM-SEL          PIC 9     VALUE 0.
       01  CUR-FORM          PIC 9     VALUE 1.

       01  D-ACCTNO          PIC 9(6).
      ******************************************************************
      *           SCREEN DISPLAY BUFFER1
      ******************************************************************
       01  DISPLAY-BUF1      VALUE SPACES.
           03  A01         PIC ZZ/ZZ/ZZ.
           03  A011        PIC X(20).
           03  A02         PIC X(20).
           03  A03         PIC X(20).
           03  A04         PIC X(60).
      * ATTORNEY PHONE:
           03  A05         PIC X(12).
           03  A06         PIC ZZ/ZZ/ZZ.
           03  A07         PIC X(20).
           03  A08         PIC ZZZ,ZZ9.99.
           03  A09         PIC ZZ/ZZ/ZZ.
           03  A10         PIC ZZ/ZZ/ZZ.
           03  A11         PIC ZZ/ZZ/ZZ.
           03  A12         PIC ZZ/ZZ/ZZ.
           03  A13         PIC ZZ/ZZ/ZZ.
           03  A14         PIC ZZ/ZZ/ZZ.
           03  A15         PIC ZZ/ZZ/ZZ.
           03  A16         PIC X.
           03  A17         PIC ZZ/ZZ/ZZ.
           03  A18         PIC ZZ/ZZ/ZZ.
           03  A19         PIC X(20).
           03  A20         PIC ZZ/ZZ/ZZ.
           03  A21         PIC X.
           03  A22         PIC ZZZ,ZZ9.99.
           03  A23         PIC ZZZ,ZZ9.99.

       01  DISPLAY-LIST1.
           03  FILLER      PIC X(28) VALUE
           "010 011 020 030 040 050 060 ".
           03  FILLER      PIC X(44) VALUE
           "070 080 090 100 110 120 130 140 150 160 170 ".
           03  FILLER      PIC X(24) VALUE
           "180 190 200 210 220 230.".

      ******************************************************************
      *    *      SCREEN DISPLAY BUFFER2
      ******************************************************************
       01  DISPLAY-BUF2    VALUE SPACES.
           03  B01         PIC ZZZ.99.
           03  B02         PIC ZZZ,ZZ9.99.
           03  B03         PIC ZZZ,ZZ9.99.
           03  B04         PIC ZZZ,ZZ9.99.
           03  B05         PIC ZZZ.99.
           03  B06         PIC X(20).
           03  B07         PIC ZZ/ZZ/ZZ.
           03  B08         PIC ZZ9.
           03  B09         PIC ZZ/ZZ/ZZ.
           03  B10         PIC ZZ/ZZ/ZZ.
           03  B11         PIC ZZ/ZZ/ZZ.
           03  B12         PIC ZZ/ZZ/ZZ.
           03  B13         PIC ZZ/ZZ/ZZ.
           03  B14         PIC ZZ/ZZ/ZZ.
           03  B15         PIC ZZ/ZZ/ZZ.
           03  B16         PIC X.
           03  B17         PIC ZZ/ZZ/ZZ.
           03  B18         PIC X(20).

       01  DISPLAY-LIST2.
           03  FILLER      PIC X(40) VALUE
           "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(32) VALUE
           "110 120 130 140 150 160 170 180.".

       01  MAX-ENTRY.
           03  FILLER        PIC X(8) VALUE "2318".
       01  MAX-TABLE REDEFINES MAX-ENTRY.
           03  MAX           PIC 99   OCCURS 2.

      ******************************************************************
      *    *   ELEMENT SPECIFICATIONS #1
      ******************************************************************
       01  SPEC-TABLE1.
      * 1  DATE SUIT AUTHORIZED
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 2  ATTORNEY NETWORK
           03  FILLER      PIC X(8) VALUE "E NA200 ".
      * 3  LOCAL ATTORNEY
           03  FILLER      PIC X(8) VALUE "E NA200 ".
      * 4  ATTORNEY ADDRESS
           03  FILLER      PIC X(8) VALUE "E NA600 ".
      * 5  ATTORNEY PHONE
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      * 6  DATE ASSIGNED
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 7  ATTORNEY ACCTNO
           03  FILLER      PIC X(8) VALUE "E NA200 ".
      * 8  PRINCIPLE BAL SENT
           03  FILLER      PIC X(8) VALUE "E NN062 ".
      * 9  ACK RECVD FROM ATTY
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *10  ATTORNEY LETTER SENT
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *11  HOLD DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *12  RESPONSE RECEIVED
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *13  SUIT DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *14  SUIT FILED
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *15  SERVICE DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *16  CUSTOMER SERVED
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *17  ANSWER FILED
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *18  COURT DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *19  TYPE OF JUDGMENT
           03  FILLER      PIC X(8) VALUE "E NA200 ".
      *20  JUDGMENT OBTAINED
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *21  COPY OF JD ON FILE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *22 JUDGMENT AMOUNT
           03  FILLER      PIC X(8) VALUE "E NN062 ".
      *23 JUDGMENT PRINCIPLE
           03  FILLER      PIC X(8) VALUE "E NN062 ".

       01  SPEC-TAB1      REDEFINES SPEC-TABLE1.
           03  SPEC-REC1  OCCURS 23 TIMES.
               05  SPEC1   PIC X(7).
               05  CHFG1   PIC X.

      ******************************************************************
      *    *   ELEMENT SPECIFICATIONS #2
      ******************************************************************
       01  SPEC-TABLE2.
      * 1  POST JUDGMENT INTRATE
           03  FILLER      PIC X(8) VALUE "ENNN032 ".
      * 2  JUDGMENT INTEREST AMT
           03  FILLER      PIC X(8) VALUE "E NN062 ".
      * 3  ATTORNEY FEES ALLOWED
           03  FILLER      PIC X(8) VALUE "E NN062 ".
      * 4  OTHER FEES ALLOWED
           03  FILLER      PIC X(8) VALUE "E NN062 ".
      * 5  JUDGEMENT INTEREST
           03  FILLER      PIC X(8) VALUE "E NN032 ".
      * 6  ASSET USED IN SUIT
           03  FILLER      PIC X(8) VALUE "E NA200 ".
      * 7  GARNISHMENT FILED
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 8  TERM OF GARNISHMENT
           03  FILLER      PIC X(8) VALUE "E NN030 ".
      * 9  INTERROGATORY SENT
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 10  INTERROGATORY ANSWERED
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 11  FIFA SENT
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 12  FIFA ANSWERED
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 13  DATE TO RECEIVE PAYMENT
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 14  FOLLOW UP DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 15  FIFA RENEWED
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 16  PAYS BEING RECEIVED FROM ATTORNEY
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 17  DATE CLOSED
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 18  REASON CLOSED
           03  FILLER      PIC X(8) VALUE "E NA200 ".

       01  SPEC-TAB2      REDEFINES SPEC-TABLE2.
           03  SPEC-REC2  OCCURS 18 TIMES.
               05  SPEC2   PIC X(7).
               05  CHFG2   PIC X.


      ******************************************************************
      *    *   ELEMENT SPECIFICATION WORKER
      ******************************************************************
       01  SPEC-TAB.
           03  SPEC-REC.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.

       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBWI/CHLEGL_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/PASSWDW.CPY".

       01  CHLEGL-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CHLEGL_DEF.CPY".
       COPY "LIBWI/CHLEG2_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/CHLEGL_WKS.CPY".
       COPY "LIBWI/CHLEG2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CHLEGL_SCN.CPY".
       COPY "LIBWI/CHLEG2_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E    D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".

           MOVE " " TO VDU-WIMENU-SELECTION.
           MOVE FORM-PATHNAME TO FORM-PATH
                                 SAVE-PATH.
           MOVE FORM-NAME(1) TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE "CHLEGL" TO VDU-FORM-NAME.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.


              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE CHLEGL-BEGIN-X * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  CHLEGL-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    CHLEGL-COLUMNS + VDU-WIN-W-MOD
                      LINES   CHLEGL-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                      HANDLE IS CHLEGL-WINDOW-HANDLE.

           MOVE "S  A240TITLE:FR." TO VDU.
           MOVE "LEGAL INFORMATION" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE EXT-WI-CL-OWNBR  TO LSL-BRNO.
           MOVE EXT-WI-CL-ACCTNO TO LSL-ACCTNO.
           MOVE 1                TO LSL-SEQNO.
           MOVE LSL1-KEY TO HOLD-LSL1-KEY.

       MAIN-MODULE.
           PERFORM FORM-RESET.

            IF NEW-REC = "Y"
               MOVE "S  A000NEW." TO VDU
               MOVE "*** NEW RECORD ***" TO VDUBUF
               PERFORM SCREENIO
               MOVE LEGEND-NEW TO LEGEND
               PERFORM SEND-LEGEND.

           PERFORM REC-LS-MODULE.

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.

           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE
             GO TO END-ROUTINE.

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.


           IF FORM-SEL NOT = 0
              PERFORM SETUP-NEW-FORM
              IF NEW-REC = "Y"
                 MOVE "S  A000NEW." TO VDU
                 MOVE "*** NEW RECORD ***" TO VDUBUF
                 PERFORM SCREENIO
                 MOVE LEGEND-NEW TO LEGEND
                 PERFORM SEND-LEGEND.

           GO TO MAIN-MODULE.

      *-----------------------------------------------------------------
       SETUP-NEW-FORM.
           EVALUATE CUR-FORM
             WHEN 1 DESTROY CHLEGL-SCREEN
             WHEN 2 DESTROY CHLEG2-SCREEN
           END-EVALUATE.

           MOVE FORM-SEL TO CUR-FORM.
           MOVE 0 TO FIRST-TIME.
           PERFORM GET-FORM.

      ******************************************************************
       REC-LS-MODULE SECTION.
      ******************************************************************
           MOVE SPACES TO IO-TYPE.

           MOVE "SN N060ACCTNO." TO VDU.
           MOVE EXT-WI-CL-ACCTNO TO D-ACCTNO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM OPEN-LSL1-FILE.
           PERFORM READ-LSL1-FILE.
           PERFORM CLOSE-LSL1-FILE.
           IF IO-FG = 0
              MOVE LSL-REC TO LSL-OR-REC
              PERFORM DISPLAY-FIELDS
              IF NEW-REC = "Y"
                 MOVE 1 TO NEW-FLAG
              ELSE
                 MOVE 0 TO NEW-FLAG
              END-IF
              GO TO END-REC-LS-MODULE.

           IF CUR-FORM NOT = 1
              MOVE 1 TO NEW-FLAG
              PERFORM DISPLAY-FIELDS
              GO TO END-REC-LS-MODULE.

           PERFORM NEW-RECORD-SETUP.

       END-REC-LS-MODULE.
           MOVE 0 TO FORM-SEL.

      ******************************************************************
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.

       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ELE < 1
              MOVE 1 TO ELE ONE.

           IF ELE NOT > MAX(CUR-FORM)
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

       MODIFY-ROUTINE.
           IF CUR-FORM = 1
              MOVE LEGEND-01 TO LEGEND
           ELSE
              MOVE LEGEND-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
      * RESTORE RECORD BACK TO VALUES AT LAST WRITE.
      *----------------------------------------------------------------
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

      *----------------------------------------------------------------
      * F7 UPDATE AND EXIT
      *----------------------------------------------------------------
           IF VDUSTATUS = "1>"
              PERFORM WRITE-MODULE
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

      *----------------------------------------------------------------
      * IF AT MENU AT BOTTOM OF THE PAGE AND ENTERING A NEW RECORD
      *   THEN YOU ONLY NEED TO HIT RETURN TO GET TO THE NEXT SCREEN
      *----------------------------------------------------------------
           IF ACTION = SPACES
              IF NEW-REC = "Y"
                 IF CUR-FORM = 1
                     MOVE  "    P2" TO ACTION
                 ELSE
                    MOVE "RWR" TO IO-TYPE
                    GO TO END-ENTRY-MODULE
                 END-IF
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

      *----------------------------------------------------------------
      * RECORD CAN ONLY BE DELETED WHILE AT PAGE 1.
      *----------------------------------------------------------------
           IF ACTION = "DELETE"
              IF CUR-FORM = 1
                 MOVE "DEL" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.


      *----------------------------------------------------------------
      * SWITCH PAGE
      *----------------------------------------------------------------
           IF ACTION = "    P1"
              IF CUR-FORM NOT = 1
                 MOVE 1 TO FORM-SEL
                 MOVE "RWR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

           IF ACTION = "    P2"
              IF CUR-FORM NOT = 2
                 MOVE 2 TO FORM-SEL
                 MOVE "RWR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.

           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.

           IF CHNO = 0
              GO TO MODIFY-ROUTINE.

           IF CHNO > MAX(CUR-FORM)
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
       ENTER-ELE.
           IF CUR-FORM = 1
              MOVE SPEC-REC1(ELE) TO SPEC-REC.

           IF CUR-FORM = 2
              MOVE SPEC-REC2(ELE) TO SPEC-REC.

           IF CHKEY NOT = ZEROS
              IF CHFG = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = CHAN
                 GO TO MODIFY-ROUTINE.

       ENTER-FIELD.
           MOVE SPEC TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.

           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

      *-----------------------------------------------------------------
       TEST-F7.
           IF VDUSTATUS = "1>"
      * IF PRESSING F7 FROM FIELD 1 AND FIRST TIME IN, JUST GET OUT
      * DONT CREATE AN EMPTY BLANK RECORD
              IF NOT (ELE = 1 AND NEW-FLAG = 1)
                 PERFORM WRITE-MODULE
              END-IF
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD.
           GO TO GOTO-FIELD1 GOTO-FIELD2
                 DEPENDING ON CUR-FORM.

      *-----------------------------------------------------------------
       GOTO-FIELD1.
           GO TO A-01 A-02 A-03 A-04 A-05 A-06 A-07 A-08 A-09 A-10
                 A-11 A-12 A-13 A-14 A-15 A-16 A-17 A-18 A-19 A-20
                 A-21 A-22 A-23
                 DEPENDING ON ELE.


      *-----------------------------------------------------------------
       GOTO-FIELD2.
           GO TO B-01 B-02 B-03 B-04 B-05 B-06 B-07 B-08 B-09 B-10
                 B-11 B-12 B-13 B-14 B-15 B-16 B-17 B-18
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *****************************************************************
      *    *      SET RECORD ELEMENTS
      *****************************************************************
       A-01.
           MOVE VDU-DATE-YYYYMMDD TO LSL-DATE-SUIT-AUTH.
       A-011.
           MOVE "E NA200011." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-01.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-011.
           MOVE VDUBUF TO LSL-DATE-SUIT-AUTH-BY.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUBUF TO LSL-ATTORNEY-NET-ASSIGN.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUBUF TO LSL-LOCAL-ATTORNEY.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUBUF TO LSL-ATTORNEY-ADDRESS.
           GO TO CREATE-ROUTINE.
       A-05.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
               GO TO ENTER-ELE.
           MOVE TEL-BUF TO LSL-ATTORNEY-PHONE.
           PERFORM SEND-PHONE.
           MOVE "S  A120050." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDU-DATE-YYYYMMDD TO LSL-DATE-ASSIGNED.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUBUF TO LSL-ATTORNEY-ACCTNO.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUNUM2 TO LSL-PRIN-BAL-SENT.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDU-DATE-YYYYMMDD TO LSL-RECEIVED-ACKNOW-DATE.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDU-DATE-YYYYMMDD TO LSL-ATTORNEY-LETTER-SENT-DATE.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDU-DATE-YYYYMMDD TO LSL-HOLD-DATE.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDU-DATE-YYYYMMDD TO LSL-RESPONSE-RECEIVED-DATE.
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDU-DATE-YYYYMMDD TO LSL-SUIT-DATE.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDU-DATE-YYYYMMDD TO LSL-SUIT-FILED-DATE.
           GO TO CREATE-ROUTINE.
       A-15.
           MOVE VDU-DATE-YYYYMMDD TO LSL-SERVICE-DATE.
           GO TO CREATE-ROUTINE.
       A-16.
           MOVE VDUBUF TO LSL-CUSTOMER-SERVED.
           IF LSL-CUSTOMER-SERVED NOT = "Y" AND NOT = "N"
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDU-DATE-YYYYMMDD TO LSL-ANSWER-FILED-DATE.
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDU-DATE-YYYYMMDD TO LSL-COURT-DATE.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDUBUF TO LSL-JUDGMENT-TYPE.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE VDU-DATE-YYYYMMDD TO LSL-JUDGMENT-OBTAINED-DATE.
           GO TO CREATE-ROUTINE.
       A-21.
           MOVE VDUBUF TO LSL-JUDGMENT-COPY-FILED.
           IF LSL-JUDGMENT-COPY-FILED NOT = "Y" AND NOT = "N"
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUNUM2 TO LSL-JUDGMENT-AMT.
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUNUM2 TO LSL-JUDGMENT-PRIN.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       B-01.
           MOVE VDUNUM2 TO LSL-POST-JUDG-INT-RATE.
           GO TO CREATE-ROUTINE.
       B-02.
           MOVE VDUNUM2 TO LSL-JUDG-INT-AMT.
           GO TO CREATE-ROUTINE.
       B-03.
           MOVE VDUNUM2 TO LSL-ATTORNEY-FEES-ALLOWED.
           GO TO CREATE-ROUTINE.
       B-04.
           MOVE VDUNUM2 TO LSL-OTHER-FEES-ALLOWED.
           GO TO CREATE-ROUTINE.
       B-05.
           MOVE VDUNUM2 TO LSL-JUDG-INT-RATE.
           GO TO CREATE-ROUTINE.
       B-06.
           MOVE VDUBUF TO LSL-ASSET-USED-IN-SUIT.
           GO TO CREATE-ROUTINE.
       B-07.
           MOVE VDU-DATE-YYYYMMDD TO LSL-GARN-FILED-DATE.
           GO TO CREATE-ROUTINE.
       B-08.
           MOVE VDUNUM0 TO LSL-GARN-TERM-DAYS.
           GO TO CREATE-ROUTINE.
       B-09.
           MOVE VDU-DATE-YYYYMMDD TO LSL-INTERROGATORY-SENT-DATE.
           GO TO CREATE-ROUTINE.
       B-10.
           MOVE VDU-DATE-YYYYMMDD TO LSL-INTERROGATORY-ANSW-DATE.
           GO TO CREATE-ROUTINE.
       B-11.
           MOVE VDU-DATE-YYYYMMDD TO LSL-FIFA-SENT-DATE.
           GO TO CREATE-ROUTINE.
       B-12.
           MOVE VDU-DATE-YYYYMMDD TO LSL-FIFA-ANSWERED-DATE.
           GO TO CREATE-ROUTINE.
       B-13.
           MOVE VDU-DATE-YYYYMMDD TO LSL-DATE-TO-RECV-PAY.
           GO TO CREATE-ROUTINE.
       B-14.
           MOVE VDU-DATE-YYYYMMDD TO LSL-FOLLOWUP-DATE.
           GO TO CREATE-ROUTINE.
       B-15.
           MOVE VDU-DATE-YYYYMMDD TO LSL-FIFA-RENEWED-DATE.
           GO TO CREATE-ROUTINE.
       B-16.
           MOVE VDUBUF TO LSL-PAYS-RECV-ATTORNEY.
           IF LSL-PAYS-RECV-ATTORNEY NOT = "Y" AND NOT = "N"
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       B-17.
           MOVE VDU-DATE-YYYYMMDD TO LSL-DATE-CLOSED.
           GO TO CREATE-ROUTINE.
       B-18.
           MOVE VDUBUF TO LSL-REASON-CLOSED.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ****************************************************
       WRITE-MODULE SECTION.
      ****************************************************
           PERFORM OPEN-LSL1-FILE.

           IF FILE-CREATED  = "N"
              GO TO WRITE-RECORD.

           MOVE LSL-REC TO LSL-CH-REC.

           IF LSL-OR-REC = LSL-CH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-LSL1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF LSL-REC NOT = LSL-OR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE LSL-CH-REC TO LSL-REC.

           PERFORM REWRITE-LSL1-FILE.
           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-LSL1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
           MOVE "Y" TO FILE-CREATED.

       END-WRITE-MODULE.
           IF NEW-REC = "Y"
              IF CUR-FORM = 2
                 MOVE "N" TO NEW-REC.

           PERFORM CLOSE-LSL1-FILE.

      ************************************************
       DELETE-MODULE SECTION.
      ************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-LSL1-FILE.
           PERFORM READ-LSL1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF LSL-REC NOT = LSL-OR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-LSL1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-LSL1-FILE.

      ***********************************************
       NEW-RECORD-SETUP SECTION.
      ***********************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "Y" TO NEW-REC.
           MOVE "N" TO FILE-CREATED.

           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE LEGEND-NEW TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM CLEAR-LSL-FILE.
           MOVE HOLD-LSL1-KEY TO LSL1-KEY.

       NEW-RECORD-EXIT.
           EXIT.

      ****************************************************
       DISPLAY-FIELDS SECTION.
      ****************************************************
           MOVE SPACES               TO DISPLAY-BUF1.
           MOVE SPACES               TO DISPLAY-BUF2.

           IF CUR-FORM NOT = 1
              GO TO SETUP-BUFFER2.

           MOVE LSL-DATE-SUIT-AUTH      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO A01.
           MOVE LSL-DATE-SUIT-AUTH-BY   TO A011.
           MOVE LSL-ATTORNEY-NET-ASSIGN TO A02.
           MOVE LSL-LOCAL-ATTORNEY      TO A03.
           MOVE LSL-ATTORNEY-ADDRESS    TO A04.
           MOVE LSL-ATTORNEY-PHONE TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT                TO A05.
           MOVE LSL-DATE-ASSIGNED       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO A06.
           MOVE LSL-ATTORNEY-ACCTNO     TO A07.
           MOVE LSL-PRIN-BAL-SENT       TO A08.
           MOVE LSL-RECEIVED-ACKNOW-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO A09.
           MOVE LSL-ATTORNEY-LETTER-SENT-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO A10.
           MOVE LSL-HOLD-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO A11.
           MOVE LSL-RESPONSE-RECEIVED-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO A12.
           MOVE LSL-SUIT-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO A13.
           MOVE LSL-SUIT-FILED-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO A14.
           MOVE LSL-SERVICE-DATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO A15.
           MOVE LSL-CUSTOMER-SERVED     TO A16.
           MOVE LSL-ANSWER-FILED-DATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO A17.
           MOVE LSL-COURT-DATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO A18.
           MOVE LSL-JUDGMENT-TYPE       TO A19.
           MOVE LSL-JUDGMENT-OBTAINED-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO A20.
           MOVE LSL-JUDGMENT-COPY-FILED TO A21.
           MOVE LSL-JUDGMENT-AMT TO A22.
           MOVE LSL-JUDGMENT-PRIN TO A23.

           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO DISPLAY-FIELDS-EXIT.

       SETUP-BUFFER2.
           IF CUR-FORM NOT = 2
              GO TO DISPLAY-FIELDS-EXIT.

           MOVE LSL-POST-JUDG-INT-RATE    TO B01.
           MOVE LSL-JUDG-INT-AMT          TO B02.
           MOVE LSL-ATTORNEY-FEES-ALLOWED TO B03.
           MOVE LSL-OTHER-FEES-ALLOWED    TO B04.
           MOVE LSL-JUDG-INT-RATE         TO B05.
           MOVE LSL-ASSET-USED-IN-SUIT    TO B06.
           MOVE LSL-GARN-FILED-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO B07.
           MOVE LSL-GARN-TERM-DAYS        TO B08.
           MOVE LSL-INTERROGATORY-SENT-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO B09.
           MOVE LSL-INTERROGATORY-ANSW-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO B10.
           MOVE LSL-FIFA-SENT-DATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO B11.
           MOVE LSL-FIFA-ANSWERED-DATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO B12.
           MOVE LSL-DATE-TO-RECV-PAY      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO B13.
           MOVE LSL-FOLLOWUP-DATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO B14.
           MOVE LSL-FIFA-RENEWED-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO B15.
           MOVE LSL-PAYS-RECV-ATTORNEY    TO B16.
           MOVE LSL-DATE-CLOSED           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO B17.
           MOVE LSL-REASON-CLOSED TO B18.

           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO DISPLAY-FIELDS-EXIT.

       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPLSLCL.CPY".


      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON CHLEGL-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           EVALUATE CUR-FORM
             WHEN 1 DISPLAY CHLEGL-SCREEN UPON CHLEGL-WINDOW-HANDLE
                 MOVE CHLEGL-FORM-FIELDS TO WR-BUF 
                 MOVE CHLEGL-FORM-LIST TO WR-LIST 
             WHEN 2 DISPLAY CHLEG2-SCREEN UPON CHLEGL-WINDOW-HANDLE
                 MOVE CHLEG2-FORM-FIELDS TO WR-BUF 
                 MOVE CHLEG2-FORM-LIST TO WR-LIST 
           END-EVALUATE.
           PERFORM CALL-WRFORM.


       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           EVALUATE CUR-FORM
              WHEN 1 MOVE CHLEGL-HELP--FILE TO HELP-LINK-FILE
                     MOVE CHLEGL-HELP--DIR  TO HELP-LINK-DIR
              WHEN 2 MOVE CHLEG2-HELP--FILE TO HELP-LINK-FILE
                     MOVE CHLEG2-HELP--DIR  TO HELP-LINK-DIR
           END-EVALUATE.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CHLEGL_EVA.CPY".
              COPY "LIBWI/CHLEG2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************
       MISC-PARAGRAPHS SECTION.
      *****************************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/ACCESS.CPY".

      *----------------------------------------------------------------
       SEND-LEGEND.
           MOVE "S  A700WSEL:D." TO VDU.
           MOVE LEGEND TO VDUBUF.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
      *----------------------------------------------------------------
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
      *-----------------------------------------------------------------
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       GET-FORM.
           MOVE FORM-NAME(CUR-FORM) TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE FORM-VDU-NAME(CUR-FORM) TO VDU-FORM-NAME.


      *************************************************
       IO-ROUTINES SECTION.
      *************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LSL1-FILE.
       COPY "LIBLP/LPLSLGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPLSL1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ************************************************
       END-ROUTINE SECTION.
      ************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.

           DISPLAY SPACES UPON CHLEGL-WINDOW-HANDLE
                                 LINE 1 COL 1 ERASE SCREEN.
           EVALUATE CUR-FORM
              WHEN 1 DESTROY CHLEGL-SCREEN
              WHEN 2 DESTROY CHLEG2-SCREEN
           END-EVALUATE.

           DESTROY CHLEGL-WINDOW-HANDLE.
           MOVE SAVE-PATH TO FORM-PATHNAME.

       STOP-RUN.
           EXIT PROGRAM.
