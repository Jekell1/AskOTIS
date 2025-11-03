       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CXMAIN.
       DATE-WRITTEN. 10-10-84
      *****************************************************
      *                 (MN)
      *  DESCRIPTION:   TEMPORARY CHECK MASTER FILE MAINTENANCE
      *
      * REV:
      *  042895 ROC  ADDED CX-DLRVNO
      *  073196 JTG  ADDED CX-DT-SEQ
      *  112596 BAH  ADDED CX-RESCISSION
      *  012097 JTG  STOPPED RECORD LOCK ON INPUT & SCAN
      *  MJD 112597 EXPANDED CX-REC TO 426 BYTES
      *  000425 BAH ADDED A SECOND TEST FOR DELETING RESCISSION CHECKS !!
      *              WISC HAD TROUBLE CLOSING ONE NIGHT, AND DELETED
      *              ALL THEIR RESCISSION CHECKS BY ACCIDENT !!
      *  000427 GLB DELETE ABOVE FOR RESCISSION WOULDNT ALLOW TO DELETE
      *              A NON RESCISSION CHECK !!
      * GLB 000810 DLR:SUBSTITUTE COPY MEMBER LPCXCL TO CLEAR CX-RECORD
      *  040330 BAH ADDED ERRLOG WHEN RECORD DELETED...
      *  CS 060626 ADD CX-EVENT-CD PR#2503 SECURITY AUTO
      * BLF 060713 ADD CX-PAYEE-NO, DEVELPMENT PR# 2633
      *  CS 070320 ALLOWED SPACES AS AN EVENT CODE PR#2503
      * MJD 131120 A30 CONVERSION COMPLETED
      *  BAH 160304 CHANGED TILLNO TO NUMERIC 99
      *   CS 170419 ACTIVATE 8 DIGIT DATES, CX-DT-DATE, CX-DATE
      * BAH 181010 GLOBAL CXFILE
      * BAH 20190930 REMOVED ACCESS-CALL PAFILE
      *  CS 20191223 SHOULD ONLY SEE YOUR BRANCH, I WAS IN BRANCH 1 AND
      *              ENTERED BRANCH 10, IT CORRECTLY GAVE ME A BRANCH 1
      *              RECORD, BUT THE SCREEN REFLECTED BRANCH 10, SEND THE  
      *              EXT-FILPATH-BRNO. ALSO, DONT ALLOW A BRANCH ENTRY IN
      *              SCANKEY THAT IS NOT = EXT-FILPATH-BRNO
      * BLM 20211023 FIX GL ACCT ENTRY SO YOU CAN ENTER OR NOT ENTER
      *              DASHES OR LEADING ZEROES
      * BAH 20220121 IF NOT VALID-TYPE, GO BACK TO ENTER-TYPE
      * BAH 20230908 REMOVED PA-FILE
      *****************************************************
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/CXMAIN.CBL S35 02/21/24-12:22:05 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01CX.CPY".
       COPY "LIBLP/LP01CX_SQL.CPY".
       COPY "LIBLP/LPWSCX.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".

      *=================================================================
      *   CXCH-REC FROM 'REPLACING LEADING' ON CX-REC. [CHANGED-REC]
      *   CXOR-REC FROM 'REPLACING LEADING' ON CX-REC. [ORIGINAL-REC]
      *=================================================================
       COPY "LIBLP/LP01CX.CPY" REPLACING LEADING "CX" BY "CXCH".
       COPY "LIBLP/LP01CX.CPY" REPLACING LEADING "CX" BY "CXOR".
      *=================================================================
       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR           PIC 9(4).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-TYPE         PIC X(2).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-SEQNO        PIC 9(4).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(10).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(12).
               05  WORK-ERRLOG-ORG-N0 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(11)-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(8).99-.
               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(12).
               05  WORK-ERRLOG-CHG-N0 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(11)-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(8).99-.

       01  HOLD-CX1-KEY.
           03  HOLD-CX-BRNO            PIC 9(4).
           03  HOLD-CX-TYPE            PIC XX.
           03  HOLD-CX-SEQNO           PIC 9(4).

       01  TEST-BUF.       
           03  TEST-CX-BRNO            PIC 9(4).
           03  TEST-CX-TYPE            PIC XX.
           03  TEST-CX-SEQNO           PIC 9(4).
          
       01  ERRCD                       PIC X      VALUE SPACES.

       01  CXMAIN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-SCAN      PIC 9       VALUE 0.
       01  DELETE-MESSAGE.
           03  FILLER      PIC X(4) VALUE "DEL ".
           03  DELETE-BRNO PIC 9(4).
           03  DELETE-TYPE PIC XX.
           03  DELETE-AMT  PIC Z(4).99.
           03  DELETE-PAYEE PIC X(13).

       01  TEST-TYPE      PIC XX.
           88  VALID-TYPE VALUE "LN" "DL" "DR" "DC" "DP" "MS".

      *    **********************************
      *    *     SCREEN DISPLAY BUFFER      *
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC Z,ZZZ,ZZZ.99-.
           03  F02         PIC X(30).
           03  F03         PIC X(24).
           03  F04         PIC X(24).
           03  F05         PIC X(15).
           03  F06         PIC X(2).
           03  F07         PIC X(10).
           03  F08         PIC X(30).
           03  F09         PIC X(12).
           03  F10         PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.
           03  F11         PIC X(12).
           03  F12         PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.
           03  F13         PIC X(12).
           03  F14         PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.
           03  F15         PIC X(12).
           03  F16         PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.
           03  F17         PIC X(12).
           03  F18         PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.
           03  F19         PIC 9(6).
           03  F191        PIC 9(6).
           03  F20         PIC 99/99/99.
           03  F21         PIC 9(8).
           03  F22         PIC 9(6).
           03  F23         PIC X.
           03  F24         PIC X.
           03  F25         PIC 99.
           03  F26         PIC Z(3)9.99.
           03  F27         PIC XX.
           03  F28         PIC 9(10).
           03  F29         PIC 9.
           03  F30         PIC 9999.
           03  F301        PIC 99/99/99.
           03  F31         PIC X.
           03  F32         PIC 99.
           03  F33         PIC X.
           03  F34         PIC X.
           03  F35         PIC XX.
       01  DISPLAY-LIST.
           03  FILLER      PIC X(40) VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(48) VALUE
               "110 120 130 140 150 160 170 180 190 191 200 210 ".
           03  FILLER      PIC X(44) VALUE
               "220 230 240 250 260 270 280 290 300 301 310 ".
           03  FILLER      PIC X(24) VALUE
               "320 330 340 350.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX             PIC 99   VALUE 35.
       01  SPEC-TABLE.
      * 1. AMT
           03  FILLER      PIC X(8) VALUE "E  S072 ".
      * 2. PAYEE
           03  FILLER      PIC X(8) VALUE "E  A300 ".
      * 3. ADR1
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      * 4. ADR2
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      * 5. CITY
           03  FILLER      PIC X(8) VALUE "E  A150 ".
      * 6. STATE
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 7. ZIP
           03  FILLER      PIC X(8) VALUE "E  A100 ".
      * 8. REASON
           03  FILLER      PIC X(8) VALUE "E  A300 ".
      * 9. GL-1
           03  FILLER      PIC X(8) VALUE "E RA120 ".
      *10. GL-AMT-1
           03  FILLER      PIC X(8) VALUE "E  S072 ".
      *11. GL-2
           03  FILLER      PIC X(8) VALUE "E RA120 ".
      *12. GL-AMT-2
           03  FILLER      PIC X(8) VALUE "E  S072 ".
      *13. GL-3
           03  FILLER      PIC X(8) VALUE "E RA120 ".
      *14. GL-AMT-3
           03  FILLER      PIC X(8) VALUE "E  S072 ".
      *15. GL-4
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      *16. GL-AMT-4
           03  FILLER      PIC X(8) VALUE "E  S072 ".
      *17. GL-5
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      *18. GL-AMT-5
           03  FILLER      PIC X(8) VALUE "E  S072 ".
      *19. CHECK NUMBER
           03  FILLER      PIC X(8) VALUE "EN N060 ".
      *20. DATE
           03  FILLER      PIC X(8) VALUE "ENNM060 ".
      *21. ACCTNO
           03  FILLER      PIC X(8) VALUE "ENNN080 ".
      *22. REFERENCE
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      *23. VOID
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *24. PAID
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *25. CLASS
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *26. DEALER NO
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      *27. PGM
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *28. BATCHID
           03  FILLER      PIC X(8) VALUE "ENNN100 ".
      *29. DEALER RESERVE NO
           03  FILLER      PIC X(8) VALUE "E  N010 ".
      *30. DT SEQNO
           03  FILLER      PIC X(8) VALUE "ENNN010 ".
      *31. RESCISSION
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *32. TILL NO
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *33. SERV  ACCT
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *34. CHECK TO BW
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *35. EVENT CODE
           03  FILLER      PIC X(8) VALUE "E  A020 ".

       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 35 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.
      *
       01  SCREEN-GL.
           03  SC-BRANCH   PIC 9(4).
           03  SC-DASH     PIC X.
           03  SC-NUMBER   PIC 9(7).
       01  SCREEN-GL-NUM REDEFINES SCREEN-GL PIC 9(12).
       01  WORK-GL         PIC 9(11).
       01  WORK-NUMBER REDEFINES WORK-GL.
           03  WORK-BR     PIC 9(4).
           03  WORK-NO     PIC 9(7).
       01  DISP-GL.
           03  DISP-BR     PIC 9(4).
           03  FILLER      PIC X VALUE "-".
           03  DISP-NO     PIC 9(7).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/CXSCANW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/CXMAIN_DEF.CPY".
       COPY "LIBMN/CXMAIN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/CXMAIN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      *****************************************
       MAIN-PROGRAM SECTION.
      *****************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "CXMAIN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CXMAIN-WINDOW-HANDLE.

       MAIN-MODULE.
      D    STOP MESS.
           PERFORM FORM-RESET.

           PERFORM GET-GPENV.

           PERFORM ENTER-KEYS-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      ******************************************************
       ENTER-KEYS-MODULE SECTION.
      ******************************************************
       ENTER-BRANCH.

           MOVE 0 TO CX-BRNO CX-SEQNO.
           MOVE SPACES TO IO-TYPE CX-TYPE.
           MOVE 0 TO NEW-FLAG.
           PERFORM FORM-RESET.
           MOVE 0 TO HOLD-CX-BRNO.

      *FORCE THE BRANCH 
           MOVE "SNNN040BRNO."   TO VDU.
           MOVE EXT-FILPATH-BRANCH TO VDUNUM0.                         
           PERFORM SCREENIO.

           MOVE VDUNUM0 TO BR-NO BR-NO HOLD-CX-BRNO CX-BRNO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.
           MOVE "S  A200BRDESC." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.
       ENTER-TYPE.
           MOVE "MESS." TO WR-LIST.
           MOVE "ENTER TYPE, F5-DETAIL SCAN, F6-SCAN" TO WR-BUF..
           PERFORM CALL-WRFORM.

           MOVE "E  A020TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO ENTER-BRANCH.
           IF VDUSTATUS = "1U"
              GO TO ENTER-BRANCH.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-ENTER-KEYS-MODULE.
           IF VDUSTATUS = "1:"
              PERFORM CXFILE-SCAN
              IF IO-FG = 9
                 GO TO ENTER-TYPE
              ELSE
                 MOVE " " TO IO-TYPE
                 GO TO READ-CXFILE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.       

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TYPE.
           MOVE VDUBUF TO CX-TYPE TEST-TYPE HOLD-CX-TYPE.
           IF NOT VALID-TYPE
              MOVE "ENTER: LN, MS, DL, DR, DC, OR DP ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TYPE.

       ENTER-SEQNO.
           MOVE "EN N040SEQNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO ENTER-BRANCH.
           IF VDUSTATUS = "1U"
              GO TO ENTER-TYPE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-ENTER-KEYS-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SEQNO.
           MOVE VDUNUM0 TO CX-SEQNO HOLD-CX-SEQNO.

       READ-CXFILE.
           MOVE CX1-KEY TO HOLD-CX1-KEY.
           PERFORM OPEN-CX1-FILE.
           PERFORM READ-CX1-FILE.
           PERFORM CLOSE-CX1-FILE.
           IF IO-FG = 0
              MOVE CX-REC TO CXOR-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.

       END-ENTER-KEYS-MODULE.
           EXIT.

      ******************************************************
       ENTER-SCANKEY SECTION.
      ******************************************************
           MOVE "F1 - RESET SCAN, F6 - CONTINUE SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-CX1-FILE.


       ENTER-KEY.
           MOVE "E RA100SCAN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCAN.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
 
           IF VDUBUF = SPACES
           IF FIRST-SCAN NOT = 0
              MOVE CX1-KEY TO HOLD-CX1-KEY
              MOVE CX-REC TO CXOR-REC
              GO TO CLOSE-SCAN
           ELSE
              GO TO ENTER-KEY.

           MOVE VDUBUF TO TEST-BUF.
           IF TEST-CX-BRNO NOT =  EXT-FILPATH-BRANCH
              MOVE "BRANCH NOT AVAILABLE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEY.                      

       READ-FILES-NEXT.
           IF FIRST-SCAN = 0
              PERFORM START-CX1-FILE
              MOVE 1 TO FIRST-SCAN.

           PERFORM READ-CX1-FILE-NEXT.
      *    UNLOCK CX-FILE.
           IF IO-FG = 9 OR (CX-BRNO NOT = CX-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCAN.

           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-FIELDS.
           GO TO ENTER-KEY.
       DISPLAY-SCAN-KEY.
           MOVE "SN N040BRNO." TO VDU.
           MOVE CX-BRNO TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A020TYPE." TO VDU.
           MOVE CX-TYPE TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SN N040SEQNO." TO VDU.
           MOVE CX-SEQNO TO VDUNUM0.
           PERFORM SCREENIO.
       CLOSE-SCAN.
           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       END-SCAN.
           PERFORM CLOSE-CX1-FILE.

      ******************************************************
       CXFILE-SCAN SECTION.
      ******************************************************
           MOVE HOLD-CX-BRNO TO CXSCANW-BRNO.
           MOVE "WI/CXSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CXSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF CXSCAN-STATUS NOT = "00"
              MOVE 9 TO IO-FG
              GO TO EXIT-CXFILE-SCAN.

           MOVE CXSCAN-CX1-KEY TO CX1-KEY.

           MOVE "TYPE." TO WR-LIST.
           MOVE CX-TYPE TO WR-BUF.
           PERFORM CALL-WRFORM.
           MOVE "SN N040SEQNO." TO VDU.
           MOVE CX-SEQNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE 0 TO IO-FG.

       EXIT-CXFILE-SCAN.
           EXIT.


      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
      *  GLB  04/27/00 TEST DELETE FOR REGULAR
	      IF CX-RESCISSION NOT = "Y"
                  MOVE "DEL" TO IO-TYPE
                  GO TO END-ENTRY-MODULE
              ELSE
                 MOVE "CHECK IS RESCISSION !!!!!!" TO MESS
                 PERFORM SEND-MESS
                 MOVE "YOU MUST TYPE 'DELETE' AGAIN OR F1-CANCEL"
                      TO MESS
                 PERFORM SEND-MESS
                 MOVE "R RA060SEL." TO VDU
                 PERFORM SCREENIO
                 IF VDUSTATUS = "10"
                    MOVE "CLR" TO IO-TYPE
                    GO TO END-ENTRY-MODULE
                 ELSE
                    MOVE VDUBUF TO ACTION
                    IF ACTION = "DELETE"
                       MOVE "DEL" TO IO-TYPE
                       GO TO END-ENTRY-MODULE
                    ELSE
                       GO TO MODIFY-ROUTINE.


           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F7.
           IF VDUSTATUS = "1>"
             IF ELE > 7 AND < 18
               MOVE 18 TO ELE
               GO TO ENTER-ELE.
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15
                 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24 F-25
                 F-26 F-27 F-28 F-29 F-30
                 F-31 F-32 F-33 F-34 F-35
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUNUM2 TO CX-AMT.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO CX-PAYEE.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO CX-ADR1.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO CX-ADR2.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO CX-CITY.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUBUF TO CX-STATE.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUBUF TO CX-ZIP.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUBUF TO CX-REASON.
           GO TO CREATE-ROUTINE.
       F-09.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO CX-GLNO(1).
           GO TO CREATE-ROUTINE.
       F-10.
           IF CX-GLNO(1) = 0
               MOVE 0 TO CX-GLAMT(1)
               GO TO CREATE-ROUTINE.
           MOVE VDUNUM2 TO CX-GLAMT(1).
           GO TO CREATE-ROUTINE.
       F-11.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO CX-GLNO(2).
           GO TO CREATE-ROUTINE.
       F-12.
           IF CX-GLNO(2) = 0
               MOVE 0 TO CX-GLAMT(2)
               GO TO CREATE-ROUTINE.
           MOVE VDUNUM2 TO CX-GLAMT(2).
           GO TO CREATE-ROUTINE.
       F-13.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO CX-GLNO(3).
           GO TO CREATE-ROUTINE.
       F-14.
           IF CX-GLNO(3) = 0
               MOVE 0 TO CX-GLAMT(3)
               GO TO CREATE-ROUTINE.
           MOVE VDUNUM2 TO CX-GLAMT(3).
           GO TO CREATE-ROUTINE.
       F-15.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO CX-GLNO(4).
           GO TO CREATE-ROUTINE.
       F-16.
           IF CX-GLNO(4) = 0
               MOVE 0 TO CX-GLAMT(4)
               GO TO CREATE-ROUTINE.
           MOVE VDUNUM2 TO CX-GLAMT(4).
           GO TO CREATE-ROUTINE.
       F-17.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO CX-GLNO(5).
           GO TO CREATE-ROUTINE.
       F-18.
           IF CX-GLNO(5) = 0
               MOVE 0 TO CX-GLAMT(5)
               GO TO CREATE-ROUTINE.
           MOVE VDUNUM2 TO CX-GLAMT(5).
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDUNUM0 TO CX-CKNO.
           MOVE "EN N060191." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUNUM0 TO CX-CKNO-NEW.
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDU-DATE-YYYYMMDD TO CX-DATE.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUNUM0 TO CX-ACCTNO.
           GO TO CREATE-ROUTINE.
       F-22.
           MOVE VDUNUM0 TO CX-REF.
           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDUBUF TO CX-VOID.
           IF CX-VOID NOT = "Y" AND CX-VOID NOT = " "
               GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-24.
           MOVE VDUBUF TO CX-PAID.
           IF CX-PAID NOT = "M"
              IF CX-PAID NOT = "B"
                 IF CX-PAID NOT = "S"
             MOVE "M - MANUAL, B - BATCH, S - SYSTEM" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-25.
           MOVE VDUNUM0 TO CX-CLASS.
           GO TO CREATE-ROUTINE.
       F-26.
           IF CX-TYPE NOT = "DL" AND NOT = "DR" AND
                       NOT = "DS" AND NOT = "DP"
              GO TO CREATE-ROUTINE.
           MOVE VDUNUM2 TO DL-DLNO.
           MOVE DL-DLNO TO CX-DLNO.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID DEALER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-27.
           MOVE VDUBUF TO CX-PGM.
           GO TO CREATE-ROUTINE.
       F-28.
           MOVE VDUNUM0 TO CX-BATCHID.
           GO TO CREATE-ROUTINE.
       F-29.
	   MOVE VDUNUM0 TO CX-DLRVNO.
	   GO TO CREATE-ROUTINE.
       F-30.
	   MOVE VDUNUM0 TO CX-DT-SEQ.
           MOVE "ENNM060301." TO VDU.
           PERFORM SCREENIO.
           MOVE VDU-DATE-YYYYMMDD TO CX-DT-DATE.
	   GO TO CREATE-ROUTINE.
       F-31.
	   MOVE VDUBUF TO CX-RESCISSION.
           IF NOT CX-RESCISSION-VALID
              GO TO ENTER-ELE.
	   GO TO CREATE-ROUTINE.
       F-32.
	   MOVE VDUNUM0 TO CX-TILLNO.
	   GO TO CREATE-ROUTINE.
       F-33.
	   MOVE VDUBUF TO CX-SERVACCT.
	   GO TO CREATE-ROUTINE.
       F-34.
	   MOVE VDUBUF TO CX-BWPROC.
           IF CX-BWPROC NOT = "Y"
              IF CX-BWPROC NOT = "N"
                 IF CX-BWPROC NOT = " "
             MOVE "Y - YES, N - NO,   - SPACE" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.
	   GO TO CREATE-ROUTINE.

       F-35.
           MOVE VDUBUF TO CX-EVENT-CD.
           IF NOT( CX-EVENT-CD NUMERIC OR
                               CX-EVENT-CD = SPACES)

              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ****************************************
       WRITE-MODULE SECTION.
      ****************************************
           PERFORM OPEN-CX1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE CX-REC TO CXCH-REC.

           IF CXOR-REC = CXCH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-CX1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF CX-REC NOT = CXOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE CXCH-REC TO CX-REC.
           PERFORM REWRITE-CX1-FILE.

           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-CX1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-CX1-FILE.

      *******************************************
       LOG-MAINTENANCE-CHGS SECTION.
      *******************************************

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE CX-BRNO   TO WORK-ERRLOG-BR.
           MOVE CX-TYPE   TO WORK-ERRLOG-TYPE.
           MOVE CX-SEQNO  TO WORK-ERRLOG-SEQNO.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF CXOR-AMT NOT = CXCH-AMT
             MOVE "01 "                 TO WORK-ERRLOG-FIELD
             MOVE "AMOUNT    "            TO WORK-ERRLOG-DESC
             MOVE CXOR-AMT              TO WORK-ERRLOG-ORG-N2
             MOVE CXCH-AMT              TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF CXOR-PAYEE NOT = CXCH-PAYEE
             MOVE "02 "                 TO WORK-ERRLOG-FIELD
             MOVE "PAYEE     "            TO WORK-ERRLOG-DESC
             MOVE CXOR-PAYEE              TO WORK-ERRLOG-ORG-ALP
             MOVE CXCH-PAYEE              TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF CXOR-ADR1 NOT = CXCH-ADR1
             MOVE "03 "                 TO WORK-ERRLOG-FIELD
             MOVE "ADDRESS 1 "            TO WORK-ERRLOG-DESC
             MOVE CXOR-ADR1              TO WORK-ERRLOG-ORG-ALP
             MOVE CXCH-ADR1              TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF CXOR-ADR2 NOT = CXCH-ADR2
             MOVE "04 "                 TO WORK-ERRLOG-FIELD
             MOVE "ADDRESS 1 "            TO WORK-ERRLOG-DESC
             MOVE CXOR-ADR2              TO WORK-ERRLOG-ORG-ALP
             MOVE CXCH-ADR2              TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF CXOR-CITY NOT = CXCH-CITY
             MOVE "05 "                  TO WORK-ERRLOG-FIELD
             MOVE "CITY      "           TO WORK-ERRLOG-DESC
             MOVE CXOR-CITY              TO WORK-ERRLOG-ORG-ALP
             MOVE CXCH-CITY              TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF CXOR-STATE NOT = CXCH-STATE
             MOVE "06 "                  TO WORK-ERRLOG-FIELD
             MOVE "STATE     "           TO WORK-ERRLOG-DESC
             MOVE CXOR-STATE             TO WORK-ERRLOG-ORG-ALP
             MOVE CXCH-STATE             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF CXOR-ZIP NOT = CXCH-ZIP
             MOVE "07 "                  TO WORK-ERRLOG-FIELD
             MOVE "ZIPCODE   "           TO WORK-ERRLOG-DESC
             MOVE CXOR-ZIP             TO WORK-ERRLOG-ORG-ALP
             MOVE CXCH-ZIP             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF CXOR-REASON NOT = CXCH-REASON
             MOVE "08 "                  TO WORK-ERRLOG-FIELD
             MOVE "REASON    "           TO WORK-ERRLOG-DESC
             MOVE CXOR-REASON             TO WORK-ERRLOG-ORG-ALP
             MOVE CXCH-REASON             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

             IF CXOR-GLNO(1) NOT = CXCH-GLNO(1)
                MOVE "09 "                  TO WORK-ERRLOG-FIELD
                MOVE "GL NUMBER "           TO WORK-ERRLOG-DESC
                MOVE CXOR-GLNO(1)         TO WORK-ERRLOG-ORG-N0
                MOVE CXCH-GLNO(1)         TO WORK-ERRLOG-CHG-N0
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-GLAMT(1) NOT = CXCH-GLAMT(1)
                MOVE "10 "                  TO WORK-ERRLOG-FIELD
                MOVE "GL AMOUNT "            TO WORK-ERRLOG-DESC
                MOVE CXOR-GLAMT(1)         TO WORK-ERRLOG-ORG-N2
                MOVE CXCH-GLAMT(1)         TO WORK-ERRLOG-CHG-N2
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-GLNO(2) NOT = CXCH-GLNO(2)
                MOVE "11 "                  TO WORK-ERRLOG-FIELD
                MOVE "GL NUMBER "           TO WORK-ERRLOG-DESC
                MOVE CXOR-GLNO(2)         TO WORK-ERRLOG-ORG-N0
                MOVE CXCH-GLNO(2)         TO WORK-ERRLOG-CHG-N0
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-GLAMT(2) NOT = CXCH-GLAMT(2)
                MOVE "12 "                  TO WORK-ERRLOG-FIELD
                MOVE "GL AMOUNT "            TO WORK-ERRLOG-DESC
                MOVE CXOR-GLAMT(2)         TO WORK-ERRLOG-ORG-N2
                MOVE CXCH-GLAMT(2)         TO WORK-ERRLOG-CHG-N2
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-GLNO(3) NOT = CXCH-GLNO(3)
                MOVE "13 "                  TO WORK-ERRLOG-FIELD
                MOVE "GL NUMBER "           TO WORK-ERRLOG-DESC
                MOVE CXOR-GLNO(3)         TO WORK-ERRLOG-ORG-N0
                MOVE CXCH-GLNO(3)         TO WORK-ERRLOG-CHG-N0
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-GLAMT(3) NOT = CXCH-GLAMT(3)
                MOVE "14 "                  TO WORK-ERRLOG-FIELD
                MOVE "GL AMOUNT "            TO WORK-ERRLOG-DESC
                MOVE CXOR-GLAMT(3)         TO WORK-ERRLOG-ORG-N2
                MOVE CXCH-GLAMT(3)         TO WORK-ERRLOG-CHG-N2
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-GLNO(4) NOT = CXCH-GLNO(4)
                MOVE "15 "                  TO WORK-ERRLOG-FIELD
                MOVE "GL NUMBER "           TO WORK-ERRLOG-DESC
                MOVE CXOR-GLNO(4)         TO WORK-ERRLOG-ORG-N0
                MOVE CXCH-GLNO(4)         TO WORK-ERRLOG-CHG-N0
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-GLAMT(4) NOT = CXCH-GLAMT(4)
                MOVE "16 "                  TO WORK-ERRLOG-FIELD
                MOVE "GL AMOUNT "            TO WORK-ERRLOG-DESC
                MOVE CXOR-GLAMT(4)         TO WORK-ERRLOG-ORG-N2
                MOVE CXCH-GLAMT(4)         TO WORK-ERRLOG-CHG-N2
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-GLNO(5) NOT = CXCH-GLNO(5)
                MOVE "17 "                  TO WORK-ERRLOG-FIELD
                MOVE "GL NUMBER "           TO WORK-ERRLOG-DESC
                MOVE CXOR-GLNO(5)         TO WORK-ERRLOG-ORG-N0
                MOVE CXCH-GLNO(5)         TO WORK-ERRLOG-CHG-N0
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-GLAMT(5) NOT = CXCH-GLAMT(5)
                MOVE "18 "                  TO WORK-ERRLOG-FIELD
                MOVE "GL AMOUNT "            TO WORK-ERRLOG-DESC
                MOVE CXOR-GLAMT(5)         TO WORK-ERRLOG-ORG-N2
                MOVE CXCH-GLAMT(5)         TO WORK-ERRLOG-CHG-N2
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-CKNO NOT = CXCH-CKNO
                MOVE "19 "             TO WORK-ERRLOG-FIELD
                MOVE "CHECK #   "      TO WORK-ERRLOG-DESC
                MOVE CXOR-CKNO         TO WORK-ERRLOG-ORG-N0
                MOVE CXCH-CKNO         TO WORK-ERRLOG-CHG-N0
                PERFORM LOG-FIELD-CHANGE.

             IF CXOR-DATE NOT = CXCH-DATE
                MOVE "20 "             TO WORK-ERRLOG-FIELD
                MOVE "DATE      "      TO WORK-ERRLOG-DESC
                MOVE CXOR-DATE         TO WORK-ERRLOG-ORG-N0
                MOVE CXCH-DATE         TO WORK-ERRLOG-CHG-N0
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-ACCTNO NOT = CXCH-ACCTNO
                MOVE "21 "             TO WORK-ERRLOG-FIELD
                MOVE "ACCTNO    "      TO WORK-ERRLOG-DESC
                MOVE CXOR-ACCTNO         TO WORK-ERRLOG-ORG-N0
                MOVE CXCH-ACCTNO         TO WORK-ERRLOG-CHG-N0
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-REF NOT = CXCH-REF
                MOVE "22 "             TO WORK-ERRLOG-FIELD
                MOVE "REFERENCE "      TO WORK-ERRLOG-DESC
                MOVE CXOR-REF         TO WORK-ERRLOG-ORG-N0
                MOVE CXCH-REF         TO WORK-ERRLOG-CHG-N0
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-VOID NOT = CXCH-VOID
                MOVE "23 "             TO WORK-ERRLOG-FIELD
                MOVE "VOIDED ?  "      TO WORK-ERRLOG-DESC
                MOVE CXOR-VOID         TO WORK-ERRLOG-ORG-ALP
                MOVE CXCH-VOID         TO WORK-ERRLOG-CHG-ALP
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-PAID NOT = CXCH-PAID
                MOVE "24 "             TO WORK-ERRLOG-FIELD
                MOVE "PAID TYPE "      TO WORK-ERRLOG-DESC
                MOVE CXOR-PAID         TO WORK-ERRLOG-ORG-ALP
                MOVE CXCH-PAID         TO WORK-ERRLOG-CHG-ALP
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-CLASS NOT = CXCH-CLASS
                MOVE "25 "             TO WORK-ERRLOG-FIELD
                MOVE "CLASS     "      TO WORK-ERRLOG-DESC
                MOVE CXOR-CLASS         TO WORK-ERRLOG-ORG-N0
                MOVE CXCH-CLASS         TO WORK-ERRLOG-CHG-N0
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-DLNO NOT = CXCH-DLNO
                MOVE "26 "             TO WORK-ERRLOG-FIELD
                MOVE "DLNO      "      TO WORK-ERRLOG-DESC
                MOVE CXOR-DLNO         TO WORK-ERRLOG-ORG-N2
                MOVE CXCH-DLNO         TO WORK-ERRLOG-CHG-N2
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-PGM NOT = CXCH-PGM
                MOVE "27 "             TO WORK-ERRLOG-FIELD
                MOVE "PGM       "      TO WORK-ERRLOG-DESC
                MOVE CXOR-PGM         TO WORK-ERRLOG-ORG-ALP
                MOVE CXCH-PGM         TO WORK-ERRLOG-CHG-ALP
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-BATCHID NOT = CXCH-BATCHID
                MOVE "28 "             TO WORK-ERRLOG-FIELD
                MOVE "BATCH ID  "      TO WORK-ERRLOG-DESC
                MOVE CXOR-BATCHID         TO WORK-ERRLOG-ORG-N0
                MOVE CXCH-BATCHID         TO WORK-ERRLOG-CHG-N0
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-DLRVNO NOT = CXCH-DLRVNO
                MOVE "29 "             TO WORK-ERRLOG-FIELD
                MOVE "DLRRSV NO "      TO WORK-ERRLOG-DESC
                MOVE CXOR-DLRVNO         TO WORK-ERRLOG-ORG-N0
                MOVE CXCH-DLRVNO         TO WORK-ERRLOG-CHG-N0
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-DT-SEQ NOT = CXCH-DT-SEQ
                MOVE "30 "             TO WORK-ERRLOG-FIELD
                MOVE "DT SEQ    "      TO WORK-ERRLOG-DESC
                MOVE CXOR-DT-SEQ         TO WORK-ERRLOG-ORG-N0
                MOVE CXCH-DT-SEQ         TO WORK-ERRLOG-CHG-N0
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-DT-DATE NOT = CXCH-DT-DATE
                MOVE "301"             TO WORK-ERRLOG-FIELD
                MOVE "DT DATE    "      TO WORK-ERRLOG-DESC
                MOVE CXOR-DT-DATE         TO WORK-ERRLOG-ORG-N0
                MOVE CXCH-DT-DATE         TO WORK-ERRLOG-CHG-N0
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-RESCISSION NOT = CXCH-RESCISSION
                MOVE "31 "              TO WORK-ERRLOG-FIELD
                MOVE "RESCISSN   "      TO WORK-ERRLOG-DESC
                MOVE CXOR-RESCISSION    TO WORK-ERRLOG-ORG-ALP
                MOVE CXCH-RESCISSION    TO WORK-ERRLOG-CHG-ALP
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-TILLNO NOT = CXCH-TILLNO
                MOVE "32 "              TO WORK-ERRLOG-FIELD
                MOVE "TILL NO    "      TO WORK-ERRLOG-DESC
                MOVE CXOR-TILLNO    TO WORK-ERRLOG-ORG-N0
                MOVE CXCH-TILLNO    TO WORK-ERRLOG-CHG-N0
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-SERVACCT NOT = CXCH-SERVACCT
                MOVE "33 "              TO WORK-ERRLOG-FIELD
                MOVE "SERV ACCT  "      TO WORK-ERRLOG-DESC
                MOVE CXOR-SERVACCT    TO WORK-ERRLOG-ORG-ALP
                MOVE CXCH-SERVACCT    TO WORK-ERRLOG-CHG-ALP
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-BWPROC NOT = CXCH-BWPROC
                MOVE "34 "              TO WORK-ERRLOG-FIELD
                MOVE "CHECK TO BW"      TO WORK-ERRLOG-DESC
                MOVE CXOR-BWPROC    TO WORK-ERRLOG-ORG-ALP
                MOVE CXCH-BWPROC    TO WORK-ERRLOG-CHG-ALP
                PERFORM LOG-FIELD-CHANGE.
             IF CXOR-EVENT-CD NOT = CXCH-EVENT-CD
                MOVE "35 "              TO WORK-ERRLOG-FIELD
                MOVE "EVENT CODE "      TO WORK-ERRLOG-DESC
                MOVE CXOR-EVENT-CD    TO WORK-ERRLOG-ORG-ALP
                MOVE CXCH-EVENT-CD    TO WORK-ERRLOG-CHG-ALP
                PERFORM LOG-FIELD-CHANGE.

      *******************************************
       LOG-FIELD-CHANGE SECTION.
      *******************************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      *******************************************
       DELETE-MODULE SECTION.
      *******************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-CX1-FILE.
           PERFORM READ-CX1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF CX-REC NOT = CXOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-CX1-FILE.

      *WRITE CHANGES FILE
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY

           MOVE CX-BRNO TO DELETE-BRNO.
           MOVE CX-TYPE TO DELETE-TYPE.
           MOVE CX-AMT TO DELETE-AMT.
           MOVE CX-PAYEE TO DELETE-PAYEE.
           MOVE DELETE-MESSAGE TO ERRLOGW-KEY.

           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       END-DELETE-MODULE.
           PERFORM CLOSE-CX1-FILE.

      ****************************************
       NEW-RECORD-SETUP SECTION.
      ****************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "NEW." TO WR-LIST.
           MOVE "*** NEW RECORD ***" TO WR-BUF.
           PERFORM CALL-WRFORM.
       CLEAR-NEW-RECORD.
           PERFORM CLEAR-CX-FILE.

           MOVE HOLD-CX1-KEY TO CX1-KEY.
           MOVE 0 TO ELE.

      ********************************************
       CHECK-GLNO SECTION.
      ********************************************
           MOVE VDUBUF TO SCREEN-GL.
           CALL "C$JUSTIFY" USING SCREEN-GL, "R".
           INSPECT SCREEN-GL REPLACING LEADING " " BY "0".
           MOVE 0 TO WORK-GL.
           IF SCREEN-GL-NUM IS NUMERIC
              MOVE SCREEN-GL-NUM TO WORK-GL
              GO TO DISPLAY-GL. 
           IF SC-DASH = "-"
              IF (SC-BRANCH NUMERIC AND SC-NUMBER NUMERIC)
                 MOVE SC-BRANCH TO WORK-BR
                 MOVE SC-NUMBER TO WORK-NO.
           IF WORK-GL NOT NUMERIC
              MOVE 9 TO IO-FG
              GO TO EXIT-CHECK-GLNO.
       DISPLAY-GL.
           MOVE "S  A120" TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0   TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           MOVE WORK-BR TO DISP-BR.
           MOVE WORK-NO TO DISP-NO.
           MOVE DISP-GL TO VDUBUF.
           PERFORM SCREENIO.
       READ-GL.
           MOVE WORK-GL TO GL-BR-ACCOUNT.
           PERFORM OPEN-GL1-FILE.
           PERFORM READ-GL1-FILE.
           PERFORM CLOSE-GL1-FILE.
           IF IO-BAD
              MOVE "INVALID G/L#" TO MESS
              PERFORM SEND-MESS.

           IF GL-BR-ACCOUNT = 0
              MOVE 0 TO IO-FG.
       EXIT-CHECK-GLNO.
           EXIT.

      ************************
       DISPLAY-FIELDS SECTION.
      ************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE CX-AMT TO F01.
           MOVE CX-PAYEE TO F02.
           MOVE CX-ADR1 TO F03.
           MOVE CX-ADR2 TO F04.
           MOVE CX-CITY TO F05.
           MOVE CX-STATE TO F06.
           MOVE CX-ZIP TO F07.
           MOVE CX-REASON TO F08.
           IF CX-GLAMT(1) NOT = 0
             MOVE CX-GLNO(1) TO WORK-GL
             MOVE WORK-BR TO DISP-BR
             MOVE WORK-NO TO DISP-NO
             MOVE DISP-GL TO F09
             MOVE CX-GLAMT(1) TO F10.
           IF CX-GLAMT(2) NOT = 0
             MOVE CX-GLNO(2) TO WORK-GL
             MOVE WORK-BR TO DISP-BR
             MOVE WORK-NO TO DISP-NO
             MOVE DISP-GL TO F11
             MOVE CX-GLAMT(2) TO F12.
           IF CX-GLAMT(3) NOT = 0
             MOVE CX-GLNO(3) TO WORK-GL
             MOVE WORK-BR TO DISP-BR
             MOVE WORK-NO TO DISP-NO
             MOVE DISP-GL TO F13
             MOVE CX-GLAMT(3) TO F14.
           IF CX-GLAMT(4) NOT = 0
             MOVE CX-GLNO(4) TO WORK-GL
             MOVE WORK-BR TO DISP-BR
             MOVE WORK-NO TO DISP-NO
             MOVE DISP-GL TO F15
             MOVE CX-GLAMT(4) TO F16.
           IF CX-GLAMT(5) NOT = 0
             MOVE CX-GLNO(5) TO WORK-GL
             MOVE WORK-BR TO DISP-BR
             MOVE WORK-NO TO DISP-NO
             MOVE DISP-GL TO F17
             MOVE CX-GLAMT(5) TO F18.
           MOVE CX-CKNO TO F19.
           MOVE CX-CKNO-NEW TO F191.
           MOVE CX-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
           MOVE DATE-MMDDYY TO F20.
           MOVE CX-ACCTNO TO F21.
           MOVE CX-REF TO F22.
           MOVE CX-VOID TO F23.
           MOVE CX-PAID TO F24.
           MOVE CX-CLASS TO F25.
           MOVE CX-DLNO TO F26.
           MOVE CX-PGM TO F27.
           MOVE CX-BATCHID TO F28.
	   MOVE CX-DLRVNO TO F29.
	   MOVE CX-DT-SEQ TO F30.
           MOVE CX-DT-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F301.
	   MOVE CX-RESCISSION TO F31.
	   MOVE CX-TILLNO TO F32.
	   MOVE CX-SERVACCT TO F33.
	   MOVE CX-BWPROC TO F34.
           MOVE CX-EVENT-CD TO F35.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO CXMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CXMAIN-SYSDATE.
           DISPLAY SPACES UPON CXMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CXMAIN-SCREEN UPON CXMAIN-WINDOW-HANDLE.
           MOVE CXMAIN-FORM-FIELDS TO WR-BUF.
           MOVE CXMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE CXMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CXMAIN-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/CXMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBLP/LPCXCL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".

      **************************************************************************
       MISC-PARAGRAPHS SECTION.
      **************************************************************************
       COPY "LIBGB/ACCESS.CPY".
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                     TO MESS.
           PERFORM SEND-MESS.

      **************************************
       IO-ROUTINES SECTION.
      **************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CX1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCXGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPCX1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************
       END-ROUTINE SECTION.
      ******************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY CXMAIN-WINDOW-HANDLE
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
