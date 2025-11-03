       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CKMAIN.
       DATE-WRITTEN.  10-10-84
      ******************************************************************
      *
      *                (MN)
      *
      * DESCRIPTION:   CHECK MASTER FILE MAINTENANCE
      *
      *=================================================================
      * REV:
      * 042895 ROC  ADDED CK-DLRVNO, DEALER RESERVE NO.
      * 060995 BAH ADDED CK-ACCTNO
      * 112295 JTG ALLOWED ZERO VOID DATE
      * 011797 JTG ALLOWED ZERO VOID DATE TO WORK
      * 012097 JTG STOPPED SCAN AND INPUT FROM LOCKING RECORD
      * 102897 GLF REMOVED RESET OF FIELDS
      * MJD 112497 EXPANDED CK-REC TO 200 BYTES
      * GLB 990413 ALLOW CK-TYPE TO BE "N" NOTES PAYABLE
      * GLB 990413 ALLOW 0 G/L ACCOUNT AND AMOUNT
      * GLB 990413 DONT VERIFY (READ) DEALER IF NOTES PAYABLE
      * GLB 000920 ADD NEW FIELD CK-BATCHID
      * GLB 000920 ADJUST MOVES OF CK-DLNO / DL-DLNO
      * GLB 001016 ADD CK-UNPAID FOR DS-UNPAID UPDATES
      * GLB 001204 ADD CHECK MASTER SCAN WINDOW
      * GLB 011004 DEFAULT BRANCH NO.
      * GLB 011011 ADD CLEARED FLAG; DISPLAY "VOID" OR "REPRINT"
      * BAH 020408 ADDED CK-CLEARED-DATE, REGMGM PR# 1726
      * BAH 030127 ADDED CHANGES LOG FOR ANY CHANGES...
      * GLB 030326 ADD CK-TILLNO;  WORLD PR# 320
      *            ADD PAYEE TYPE "DD" IS VALID
      * GLB 030613 CK-VOID; ACCEPT "N" AND ACCEPT " "
      *            BOTH ARE VALID INDICATING "NOT VOIDED"
      * BAH 031203 ERRLOG FOR DELETE
      * BLF 050308 WAS MISSING CALL FOR ERRLOG ON DELETE, WASN'T
      *            LOGGING
      *  CS 060626 ADDED CK-EVENT-CD PR#2503 SECURITY AUTO
      * BLF 060713 ADDED CK-PAYEE-NO, FIELD 32, DEVELOPMENT PR# 2633
      *  CS 070314 ALLOWED SPACES IN CK-EVENT-CD PR#2503  BARB
      * BLF 070918 CHANGE IO FOR PAYEE FILE FROM LPPA1IN TO LPPA1RN
      *  CS 170418 ACTIVATE 8 DIGIT DATES, CK-DATE,CK-VOID-DATE,CK-CLEARED-DATE
      * BAH 180601 ADDED ERRLOG
      * BAH 20190930 REMOVED ACCESS-CALL PAFILE
      *  CS 20200109 SHOULD ONLY SEE YOUR BRANCH SO I AM FORCING THE BRANCH
      *              SENDING EXT-FILPATH-BRNO.  ALSO, DONT ALLOW AN ENTRY
      *              BRANCH IN SCANKEY NOT = EXT-FILPATH-BRNO.
      * BAH 20200702 MOVED OPEN-BR-FILE #TST001
      * BLM 20210924 TOOK DASHES OF WORK-KEY, FIELD 25 SSNO WAS 
      *              DDD-DD-DDDD IN A15, WAS FORCING ZERO HERE
      * BLM 20211022 FIX GL NUMBER ENTRIES SO YOU CAN ENTER WITH OR
      *              WITHOUT DASHES OR LEADING ZEROS
      * BAH 20230908 REMOVED PA-FILE, FIELD 32
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

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./MN/CKMAIN.CBL S34 10/22/21-14:42:24 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".

      *=================================================================
      *   CKCH-REC FROM 'REPLACING LEADING' ON CK-REC. [CHANGED-REC]
      *   CKOR-REC FROM 'REPLACING LEADING' ON CK-REC. [ORIGINAL-REC]
      *=================================================================
       COPY "LIBLP/LP01CK.CPY" REPLACING LEADING "CK" BY "CKCH".
       COPY "LIBLP/LP01CK.CPY" REPLACING LEADING "CK" BY "CKOR".
      *=================================================================
       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR           PIC 9(4).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-NUMBER       PIC Z(7).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  WORK-ERRLOG-FIELDN REDEFINES WORK-ERRLOG-FIELD PIC 9(3).
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

       01  HOLD-CK1-KEY.
           03  HOLD-CK-BRNO            PIC 9(4).
           03  HOLD-CK-NO              PIC 9(6).

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  ERRCD                       PIC X      VALUE SPACES.

       01  CKMAIN-WINDOW-HANDLE        PIC X(10).
       01  SUB                         PIC 999     VALUE 0.
       01  ONE                         PIC S9.
       01  ELE                         PIC 9(2).

       01  ACTION.
           03  CHKEY                   PIC X(4).
           03  CHNO                    PIC 9(2).

       01  IO-TYPE                     PIC X(3).
       01  NEW-FLAG                    PIC 9.
       01  FIRST-TIME                  PIC 9       VALUE 0.

       01  WORK-KEY.
           03  WK-1                    PIC X(3).
           03  WK-2                    PIC XX.
           03  WK-3                    PIC X(4).

       01  SOCSEC-KEY                  PIC 9(9).
       01  SS-KEY            REDEFINES SOCSEC-KEY.
           03  SS-1                    PIC X(3).
           03  SS-2                    PIC XX.
           03  SS-3                    PIC X(4).

       01  TEST-PAYEE                  PIC XX.
           88  VALIDP VALUE
           "LN" , "DL" , "DR" , "DS" , "DP" , "MS" "DD".

       01  MESS-LIST                   PIC X(5) VALUE "MESS.".

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

      ******************************************************************
      *
      *    D I S P L A Y - W O R K E R S
      *
      ******************************************************************
       01  DISPLAY-BUF                 VALUE SPACES.
           03  F01                     PIC XX.
           03  F02                     PIC 99/99/99.
           03  F03                     PIC Z,ZZZ,ZZZ.99-.
           03  F04                     PIC X(30).
           03  F05                     PIC 9(6).
           03  F06                     PIC X(30).
           03  F07                     PIC X(12).
           03  F08                     PIC Z,ZZZ,ZZZ.99-  BLANK ZERO.
           03  F09                     PIC X(12).
           03  F10                     PIC Z,ZZZ,ZZZ.99-  BLANK ZERO.
           03  F11                     PIC X(12).
           03  F12                     PIC Z,ZZZ,ZZZ.99-  BLANK ZERO.
           03  F13                     PIC X(12).
           03  F14                     PIC Z,ZZZ,ZZZ.99-  BLANK ZERO.
           03  F15                     PIC X(12).
           03  F16                     PIC Z,ZZZ,ZZZ.99-  BLANK ZERO.
           03  F17                     PIC X.
           03  F171                    PIC X(9).
           03  F18                     PIC 99/99/99   BLANK ZERO.
           03  F19                     PIC X.
           03  F20                     PIC 99.
           03  F21                     PIC Z(3)9.99.
           03  F22                     PIC XX.
           03  F23                     PIC 9.
           03  F24                     PIC Z(8).
           03  F25                     PIC 999/99/9999  BLANK ZERO.
           03  F26                     PIC 9(10)    BLANK ZERO.
           03  F27                     PIC X.
           03  F28                     PIC X.
           03  F29                     PIC 99/99/99 BLANK ZERO.
           03  F30                     PIC 99.
           03  F31                     PIC XX.

      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER      PIC X(52) VALUE
               "010 020 030 040 050 060 070 080 090 100 110 120 130 ".
           03  FILLER      PIC X(52) VALUE
               "140 150 160 170 171 180 190 200 210 220 230 240 250 ".
           03  FILLER      PIC X(32) VALUE
               "260 270 280 290 300 310.".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      ******************************************************************
       78  MAX                         VALUE 31.
      *=================================================================
       01  SPEC-TABLE.
      * 1. TYPE
           03  FILLER                  PIC X(8) VALUE "E  A020N".
      * 2. DATE
           03  FILLER                  PIC X(8) VALUE "E NM060N".
      * 3. AMT
           03  FILLER                  PIC X(8) VALUE "E NS072N".
      * 4. PAYEE
           03  FILLER                  PIC X(8) VALUE "E  A300N".
      * 5. REFERENCE
           03  FILLER                  PIC X(8) VALUE "ENNN060N".
      * 6. REASON
           03  FILLER                  PIC X(8) VALUE "E  A300N".
      * 7. GL-1
           03  FILLER                  PIC X(8) VALUE "E RA120N".
      * 8. GL-AMT-1
           03  FILLER                  PIC X(8) VALUE "E NS072N".
      * 9. GL-2
           03  FILLER                  PIC X(8) VALUE "E RA120N".
      *10. GL-AMT-2
           03  FILLER                  PIC X(8) VALUE "E NS072N".
      *11. GL-3
           03  FILLER                  PIC X(8) VALUE "E RA120N".
      *12. GL-AMT-3
           03  FILLER                  PIC X(8) VALUE "E NS072N".
      *13. GL-4
           03  FILLER                  PIC X(8) VALUE "E RA120N".
      *14. GL-AMT-4
           03  FILLER                  PIC X(8) VALUE "E NS072N".
      *15. GL-5
           03  FILLER                  PIC X(8) VALUE "E RA120N".
      *16. GL-AMT-5
           03  FILLER                  PIC X(8) VALUE "E NS072N".
      *17. VOID
           03  FILLER                  PIC X(8) VALUE "E  A010N".
      *18. VOID DATE
           03  FILLER                  PIC X(8) VALUE "ERNM060N".
      *19. PAID
           03  FILLER                  PIC X(8) VALUE "E  A010N".
      *20. CLASS
           03  FILLER                  PIC X(8) VALUE "ENNN020N".
      *21. DEALER NO
           03  FILLER                  PIC X(8) VALUE "ENNN042N".
      *22. PGM
           03  FILLER                  PIC X(8) VALUE "E  A010N".
      *23. DEALER RESERVE NO
	   03  FILLER                  PIC X(8) VALUE "E NN010N".
      *24. ACCOUNT NUMBER
	   03  FILLER                  PIC X(8) VALUE "ENNN080N".
      *25. SSNO
	   03  FILLER                  PIC X(8) VALUE "E  A110N".
      *26. BATCHID
	   03  FILLER                  PIC X(8) VALUE "ENNN100N".
      *27. UNPAID FLAG FOR DS-UNPAID
           03  FILLER                  PIC X(8) VALUE "E  A010N".
      *28. CLEARED FLAG FOR CHECK RECON
           03  FILLER                  PIC X(8) VALUE "E  A010N".
      *29. CLEARED DATE
           03  FILLER                  PIC X(8) VALUE "ERNM060N".
      *30. TILL NO FOR CASH DRAWER CHECKS
           03  FILLER                  PIC X(8) VALUE "ENNN020N".
      *31. EVENT CODE
           03  FILLER                  PIC X(8) VALUE "E  A020N".

      *=================================================================
       01  SPEC-TAB                    REDEFINES SPEC-TABLE.
           03  SPEC-REC                OCCURS MAX TIMES.
               05  SPEC                PIC X(7).
               05  CHFG                PIC X.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/CKSCANW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/CKMAIN_DEF.CPY".
       COPY "LIBMN/CKMAIN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/CKMAIN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "CKMAIN"                   TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CKMAIN-WINDOW-HANDLE.

           PERFORM OPEN-BR-FILE.

       MAIN-MODULE.
      D    STOP MESS.
           PERFORM FORM-RESET.

           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME
              MOVE 0 TO HOLD-CK-BRNO.

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

      ******************************************************************
       ENTER-KEYS-MODULE SECTION.
      ******************************************************************
       ENTER-BRANCH.
           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO NEW-FLAG.

           MOVE "SNNN040BRNO."   TO VDU.
           MOVE EXT-FILPATH-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

           IF VDUNUM0 = 0
              MOVE "SN N040BRNO." TO VDU
              MOVE HOLD-CK-BRNO TO VDUNUM0
              PERFORM SCREENIO.

           MOVE VDUNUM0 TO BR-NO HOLD-CK-BRNO CK-BRNO.

           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
               MOVE "INVALID BRANCH" TO MESS
               PERFORM SEND-MESS
               MOVE "END" TO IO-TYPE
               GO TO END-ENTER-KEYS-MODULE.
           MOVE "S  A200BRDESC." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

       ENTER-CHECK.
           MOVE "F1-BRANCH, F6-SCAN WINDOW, F5-SCAN" TO MESS.
           MOVE MESS-LIST TO WR-LIST.
           MOVE MESS      TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE "ENNN060CHECK." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS = "1:"
              PERFORM ENTER-SCAN
              GO TO END-ENTER-KEYS-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM CKFILE-SCAN
              IF IO-FG = 9
                 GO TO ENTER-CHECK
              ELSE
                 GO TO READ-CHECK-FILE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-CHECK.
           IF VDUNUM0 = 0
              GO TO ENTER-CHECK.
           MOVE VDUNUM0 TO CK-NO.

       READ-CHECK-FILE.
           MOVE SPACES TO MESS.
           MOVE MESS-LIST TO WR-LIST.
           MOVE MESS      TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE CK1-KEY TO HOLD-CK1-KEY.
           PERFORM OPEN-CK1-FILE.
           PERFORM READ-CK1-FILE.
           PERFORM CLOSE-CK1-FILE.
           IF IO-FG = 0
               MOVE CK-REC TO CKOR-REC
               PERFORM DISPLAY-FIELDS
           ELSE
               PERFORM NEW-RECORD-SETUP.

       END-ENTER-KEYS-MODULE.
           EXIT.

      ******************************************************************
       ENTER-SCAN SECTION.
      ******************************************************************

           MOVE " ENTER CHECK #   OR   F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-CK1-FILE.
 
           MOVE CK-PATH-OWNBR TO CK-BRNO.
           MOVE 0            TO CK-NO.

           PERFORM START-CK1-FILE.

           GO TO READ-FILES-NEXT.

      *-----------------------------------------------------------------
       ENTER-KEY.
           MOVE "EN N060SCAN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCAN.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUNUM0 = 0
              MOVE CK-REC TO CKOR-REC
              GO TO CLOSE-SCAN.

           MOVE CK-PATH-OWNBR TO CK-BRNO.
           MOVE VDUNUM0       TO CK-NO.

           PERFORM START-CK1-FILE.

       READ-FILES-NEXT.
           PERFORM READ-CK1-FILE-NEXT.
      *    UNLOCK CK-FILE.
           IF IO-FG = 9 OR (CK-BRNO NOT = CK-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCAN.

           MOVE "SN N060CHECK." TO VDU.
           MOVE CK-NO TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.

           GO TO ENTER-KEY.

       CLOSE-SCAN.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

       END-SCAN.
           PERFORM CLOSE-CK1-FILE.

      ******************************************************************
       CKFILE-SCAN SECTION.
      ******************************************************************
    
           MOVE "Y"          TO CKSCAN-NOSTOP-FG.
           MOVE HOLD-CK-BRNO TO CKSCANW-BRNO.
           MOVE 0            TO CK-NO.
           MOVE "WI/CKSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CKSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CKSCAN-STATUS NOT = "00"
              MOVE 9 TO IO-FG
           ELSE
              MOVE 0 TO IO-FG
              MOVE CKSCAN-CK1-KEY TO CK1-KEY
              MOVE "SN N040BRNO." TO VDU
              MOVE CK-BRNO TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SN N060CHECK." TO VDU
              MOVE CK-NO TO VDUNUM0
              PERFORM SCREENIO.


      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *-----------------------------------------------------------------
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      ******************************************************************
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

      *-----------------------------------------------------------------
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           CALL "C$JUSTIFY" USING ACTION "R".
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
             MOVE "DEL" TO IO-TYPE
             GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF NEW-FLAG = 0
              IF CHFG(ELE) = "N"
                 IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                    GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       TEST-F7.
           IF VDUSTATUS = "1>"
             IF ELE > 7 AND < 18
               MOVE 18 TO ELE
               GO TO ENTER-ELE.

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
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15
                 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24 F-25
                 F-26 F-27 F-28 F-29 F-30
                 F-31
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *=================================================================
      *
      *    SET RECORD ELEMENTS
      *
      *=================================================================
       F-01.
           MOVE VDUBUF TO CK-TYPE.
           MOVE CK-TYPE TO TEST-PAYEE.
      * GLB  04/13, ALLOW "N" FOR NOTES PAYABLE
           IF CK-TYPE = "N" GO TO CREATE-ROUTINE.

           IF VALIDP
              GO TO CREATE-ROUTINE.

           PERFORM INVALID-CHECK-TYPE.
           GO TO ENTER-ELE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDU-DATE-YYYYMMDD TO CK-DATE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUNUM2 TO CK-AMT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO CK-PAYEE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUNUM0 TO CK-REF.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDUBUF TO CK-REASON.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO CK-GLNO(1).
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-08.
           IF CK-GLNO(1) = 0
               MOVE 0 TO CK-GLAMT(1)
               GO TO CREATE-ROUTINE.
           MOVE VDUNUM2 TO CK-GLAMT(1).
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-09.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO CK-GLNO(2).
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-10.
           IF CK-GLNO(2) = 0
               MOVE 0 TO CK-GLAMT(2)
               GO TO CREATE-ROUTINE.
           MOVE VDUNUM2 TO CK-GLAMT(2).
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-11.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO CK-GLNO(3).
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-12.
           IF CK-GLNO(3) = 0
               MOVE 0 TO CK-GLAMT(3)
               GO TO CREATE-ROUTINE.
           MOVE VDUNUM2 TO CK-GLAMT(3).
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-13.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO CK-GLNO(4).
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-14.
           IF CK-GLNO(4) = 0
               MOVE 0 TO CK-GLAMT(4)
               GO TO CREATE-ROUTINE.
           MOVE VDUNUM2 TO CK-GLAMT(4).
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-15.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO CK-GLNO(5).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-16.
           IF CK-GLNO(5) = 0
               MOVE 0 TO CK-GLAMT(5)
               GO TO CREATE-ROUTINE.
           MOVE VDUNUM2 TO CK-GLAMT(5).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-17.
           MOVE VDUBUF TO CK-VOID.
           IF CK-VOID NOT = "Y" AND CK-VOID NOT = " "
              AND CK-VOID NOT = "N"
              AND CK-VOID NOT = "R"
              MOVE "Y(YES VOIDED), R(REPRINT), N OR SPACE(ORIGINAL)"
                 TO MESS PERFORM SEND-MESS
               GO TO ENTER-ELE.
           MOVE "S  A000171." TO VDU.
           MOVE SPACES TO VDUBUF.
           IF CK-VOID = "V" MOVE "VOIDED   " TO VDUBUF.
           IF CK-VOID = "R" MOVE "REPRINTED" TO VDUBUF.
           IF CK-VOID = " " MOVE "ORIGINAL " TO VDUBUF.
           IF CK-VOID = "N" MOVE "ORIGINAL " TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-18.
           IF VDUNUM0 = 0
              IF CK-VOID = "Y"
                 MOVE "MUST HAVE DATE IF CHECK IS VOIDED" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
                 MOVE VDU-DATE-YYYYMMDD TO CK-VOID-DATE
                 GO TO CREATE-ROUTINE
           ELSE
              IF CK-VOID NOT = "Y"
                 MOVE "DATE ONLY ALLOWED IF CHECK IS VOIDED" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           MOVE VDU-DATE-YYYYMMDD TO CK-VOID-DATE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-19.
           MOVE VDUBUF TO CK-PAID.
           IF CK-PAID NOT = "M"
              IF CK-PAID NOT = "B"
                 IF CK-PAID NOT = "S"
           MOVE "M - MANUAL, B - BATCH, S - SYSTEM" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-20.
           MOVE VDUNUM0 TO CK-CLASS.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-21.
           MOVE VDUNUM2 TO DL-DLNO.
      *    MOVE DL-NUM TO CK-DLNO.
           MOVE DL-DLNO TO CK-DLNO.
      * GLB 04/13  SKIP IF NOTES PAYABLE
           IF CK-TYPE = "N"
              GO TO CREATE-ROUTINE.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID DEALER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-22.
           MOVE VDUBUF TO CK-PGM.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-23.
           MOVE VDUNUM0 TO CK-DLRVNO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-24.
           MOVE VDUNUM0 TO CK-ACCTNO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-25.
           MOVE VDUBUF TO WORK-KEY.
           MOVE WK-1 TO SS-1.
           MOVE WK-2 TO SS-2.
           MOVE WK-3 TO SS-3.
           IF SS-KEY NOT NUMERIC
              MOVE 0 TO SS-KEY.
           MOVE SOCSEC-KEY TO CK-SSNO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-26.
           MOVE VDUNUM0 TO CK-BATCHID.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-27.
           MOVE VDUBUF TO CK-UNPAID.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-28.
           MOVE VDUBUF TO CK-CLEARED-FG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-29.
           MOVE VDU-DATE-YYYYMMDD TO CK-CLEARED-DATE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-30.
           MOVE VDUNUM0 TO CK-TILLNO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-31.
           MOVE VDUBUF TO CK-EVENT-CD.
           IF NOT (CK-EVENT-CD NUMERIC OR
                           CK-EVENT-CD = SPACES)
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.


       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-CK1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE CK-REC TO CKCH-REC.

           IF CKOR-REC = CKCH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-CK1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF CK-REC NOT = CKOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE CKCH-REC TO CK-REC.
           PERFORM REWRITE-CK1-FILE.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-CK1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-CK1-FILE.

      *******************************************
       LOG-MAINTENANCE-CHGS SECTION.
      *******************************************

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE CK-BRNO   TO WORK-ERRLOG-BR.
           MOVE CK-NO     TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF CKOR-TYPE NOT = CKCH-TYPE
             MOVE "01 "                   TO WORK-ERRLOG-FIELD
             MOVE "TYPE     "            TO WORK-ERRLOG-DESC
             MOVE CKOR-TYPE              TO WORK-ERRLOG-ORG-ALP
             MOVE CKCH-TYPE              TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF CKOR-DATE NOT = CKCH-DATE
             MOVE "02 "                   TO WORK-ERRLOG-FIELD
             MOVE "DATE     "            TO WORK-ERRLOG-DESC
             MOVE CKOR-DATE              TO WORK-ERRLOG-ORG-N0
             MOVE CKCH-DATE              TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF CKOR-AMT NOT = CKCH-AMT
             MOVE "03 "                   TO WORK-ERRLOG-FIELD
             MOVE "AMOUNT   "            TO WORK-ERRLOG-DESC
             MOVE CKOR-AMT              TO WORK-ERRLOG-ORG-N2
             MOVE CKCH-AMT              TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF CKOR-PAYEE NOT = CKCH-PAYEE
             MOVE "04 "                   TO WORK-ERRLOG-FIELD
             MOVE "PAYEE    "            TO WORK-ERRLOG-DESC
             MOVE CKOR-PAYEE              TO WORK-ERRLOG-ORG-ALP
             MOVE CKCH-PAYEE              TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF CKOR-REF NOT = CKCH-REF
             MOVE "05 "                  TO WORK-ERRLOG-FIELD
             MOVE "REFERENCE"            TO WORK-ERRLOG-DESC
             MOVE CKOR-REF               TO WORK-ERRLOG-ORG-ALP
             MOVE CKCH-REF               TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF CKOR-REASON NOT = CKCH-REASON
             MOVE "06 "                  TO WORK-ERRLOG-FIELD
             MOVE "REASON   "            TO WORK-ERRLOG-DESC
             MOVE CKOR-REASON            TO WORK-ERRLOG-ORG-ALP
             MOVE CKCH-REASON            TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              IF CKOR-GLNO(SUB) NOT = CKCH-GLNO(SUB)
                COMPUTE WORK-ERRLOG-FIELDN = SUB + 6
                MOVE "GL NUMBER "           TO WORK-ERRLOG-DESC
                MOVE CKOR-GLNO(SUB)         TO WORK-ERRLOG-ORG-N0
                MOVE CKCH-GLNO(SUB)         TO WORK-ERRLOG-CHG-N0
                PERFORM LOG-FIELD-CHANGE
              END-IF
              IF CKOR-GLAMT(SUB) NOT = CKCH-GLAMT(SUB)
                COMPUTE WORK-ERRLOG-FIELDN = SUB + 6
                MOVE "GL AMOUNT "            TO WORK-ERRLOG-DESC
                MOVE CKOR-GLAMT(SUB)         TO WORK-ERRLOG-ORG-N2
                MOVE CKCH-GLAMT(SUB)         TO WORK-ERRLOG-CHG-N2
                PERFORM LOG-FIELD-CHANGE
              END-IF
           END-PERFORM.

           IF CKOR-VOID NOT = CKCH-VOID
             MOVE "17 "                TO WORK-ERRLOG-FIELD
             MOVE "VOIDED "            TO WORK-ERRLOG-DESC
             MOVE CKOR-VOID            TO WORK-ERRLOG-ORG-ALP
             MOVE CKCH-VOID            TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF CKOR-VOID-DATE NOT = CKCH-VOID-DATE
             MOVE "18 "                TO WORK-ERRLOG-FIELD
             MOVE "VOID DATE "            TO WORK-ERRLOG-DESC
             MOVE CKOR-VOID-DATE            TO WORK-ERRLOG-ORG-N0
             MOVE CKCH-VOID-DATE            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF CKOR-PAID NOT = CKCH-PAID
             MOVE "19 "                TO WORK-ERRLOG-FIELD
             MOVE "PAID TYPE "         TO WORK-ERRLOG-DESC
             MOVE CKOR-PAID            TO WORK-ERRLOG-ORG-ALP
             MOVE CKCH-PAID            TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF CKOR-CLASS NOT = CKCH-CLASS
             MOVE "20 "                TO WORK-ERRLOG-FIELD
             MOVE "CLASS     "         TO WORK-ERRLOG-DESC
             MOVE CKOR-CLASS            TO WORK-ERRLOG-ORG-N0
             MOVE CKCH-CLASS            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF CKOR-DLNO NOT = CKCH-DLNO
             MOVE "21 "                TO WORK-ERRLOG-FIELD
             MOVE "DEALER NO "         TO WORK-ERRLOG-DESC
             MOVE CKOR-DLNO            TO WORK-ERRLOG-ORG-N2
             MOVE CKCH-DLNO            TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF CKOR-PGM NOT = CKCH-PGM
             MOVE "22 "                TO WORK-ERRLOG-FIELD
             MOVE "PROGRAM   "         TO WORK-ERRLOG-DESC
             MOVE CKOR-PGM            TO WORK-ERRLOG-ORG-ALP
             MOVE CKCH-PGM            TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF CKOR-DLRVNO NOT = CKCH-DLRVNO
             MOVE "23 "                TO WORK-ERRLOG-FIELD
             MOVE "DLRRSV NO"         TO WORK-ERRLOG-DESC
             MOVE CKOR-DLRVNO            TO WORK-ERRLOG-ORG-N0
             MOVE CKCH-DLRVNO            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF CKOR-ACCTNO NOT = CKCH-ACCTNO
             MOVE "24 "                 TO WORK-ERRLOG-FIELD
             MOVE "ACCTNO   "           TO WORK-ERRLOG-DESC
             MOVE CKOR-ACCTNO            TO WORK-ERRLOG-ORG-N0
             MOVE CKCH-ACCTNO            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF CKOR-SSNO NOT = CKCH-SSNO
             MOVE "25 "                 TO WORK-ERRLOG-FIELD
             MOVE "SSNO   "           TO WORK-ERRLOG-DESC
             MOVE CKOR-SSNO            TO WORK-ERRLOG-ORG-N0
             MOVE CKCH-SSNO            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF CKOR-BATCHID NOT = CKCH-BATCHID
             MOVE "26 "                 TO WORK-ERRLOG-FIELD
             MOVE "BATCHID   "           TO WORK-ERRLOG-DESC
             MOVE CKOR-BATCHID            TO WORK-ERRLOG-ORG-N0
             MOVE CKCH-BATCHID            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF CKOR-UNPAID NOT = CKCH-UNPAID
             MOVE "27 "                 TO WORK-ERRLOG-FIELD
             MOVE "UNPAID   "           TO WORK-ERRLOG-DESC
             MOVE CKOR-UNPAID            TO WORK-ERRLOG-ORG-ALP
             MOVE CKCH-UNPAID            TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF CKOR-CLEARED-FG NOT = CKCH-CLEARED-FG
             MOVE "28 "                 TO WORK-ERRLOG-FIELD
             MOVE "CLEARED FG"           TO WORK-ERRLOG-DESC
             MOVE CKOR-CLEARED-FG            TO WORK-ERRLOG-ORG-ALP
             MOVE CKCH-CLEARED-FG            TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF CKOR-CLEARED-DATE NOT = CKCH-CLEARED-DATE
             MOVE "29 "                  TO WORK-ERRLOG-FIELD
             MOVE "CLEARED FG"           TO WORK-ERRLOG-DESC
             MOVE CKOR-CLEARED-DATE      TO WORK-ERRLOG-ORG-N0
             MOVE CKCH-CLEARED-DATE      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF CKOR-TILLNO NOT = CKCH-TILLNO
             MOVE "30 "                  TO WORK-ERRLOG-FIELD
             MOVE "DRAWER NO"           TO WORK-ERRLOG-DESC
             MOVE CKOR-TILLNO      TO WORK-ERRLOG-ORG-N0
             MOVE CKCH-TILLNO      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF CKOR-EVENT-CD NOT = CKCH-EVENT-CD
             MOVE "31 "                  TO WORK-ERRLOG-FIELD
             MOVE "EVENT CD"           TO WORK-ERRLOG-DESC
             MOVE CKOR-EVENT-CD      TO WORK-ERRLOG-ORG-ALP
             MOVE CKCH-EVENT-CD      TO WORK-ERRLOG-CHG-ALP
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


      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-CK1-FILE.
           PERFORM READ-CK1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF CK-REC NOT = CKOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-CK1-FILE.

      * CODE TO WRITE CHANGE FILE.
           MOVE "C" TO ERRLOGW-ACTION.
      *WRITE CHANGES FILE
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY.
           STRING CK-BRNO,CK1-KEY, " ", "DELETED" DELIMITED BY SIZE
                INTO ERRLOGW-KEY.

           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       END-DELETE-MODULE.
           PERFORM CLOSE-CK1-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-CK-FILE.
           MOVE HOLD-CK1-KEY TO CK1-KEY.
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

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE CK-TYPE TO F01.
           MOVE CK-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F02.

           MOVE CK-AMT TO F03.
           MOVE CK-PAYEE TO F04.
           MOVE CK-REF TO F05.
           MOVE CK-REASON TO F06.
           IF CK-GLAMT(1) NOT = 0
             MOVE CK-GLNO(1) TO WORK-GL
             MOVE WORK-BR TO DISP-BR
             MOVE WORK-NO TO DISP-NO
             MOVE DISP-GL TO F07
             MOVE CK-GLAMT(1) TO F08.
           IF CK-GLAMT(2) NOT = 0
             MOVE CK-GLNO(2) TO WORK-GL
             MOVE WORK-BR TO DISP-BR
             MOVE WORK-NO TO DISP-NO
             MOVE DISP-GL TO F09
             MOVE CK-GLAMT(2) TO F10.
           IF CK-GLAMT(3) NOT = 0
             MOVE CK-GLNO(3) TO WORK-GL
             MOVE WORK-BR TO DISP-BR
             MOVE WORK-NO TO DISP-NO
             MOVE DISP-GL TO F11
             MOVE CK-GLAMT(3) TO F12.
           IF CK-GLAMT(4) NOT = 0
             MOVE CK-GLNO(4) TO WORK-GL
             MOVE WORK-BR TO DISP-BR
             MOVE WORK-NO TO DISP-NO
             MOVE DISP-GL TO F13
             MOVE CK-GLAMT(4) TO F14.
           IF CK-GLAMT(5) NOT = 0
             MOVE CK-GLNO(5) TO WORK-GL
             MOVE WORK-BR TO DISP-BR
             MOVE WORK-NO TO DISP-NO
             MOVE DISP-GL TO F15
             MOVE CK-GLAMT(5) TO F16.
           MOVE CK-VOID TO F17.
           MOVE SPACES TO F171.
           IF CK-VOID = "V" MOVE "VOIDED" TO F171.
           IF CK-VOID = "R" MOVE "REPRINTED" TO F171.
           MOVE CK-VOID-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F18.

           MOVE CK-PAID TO F19.
           MOVE CK-CLASS TO F20.
           MOVE CK-DLNO TO F21.

           MOVE CK-PGM TO F22.
	   MOVE CK-DLRVNO TO F23.
           MOVE CK-ACCTNO TO F24.
           IF CK-SSNO > 0
               MOVE CK-SSNO TO F25
               INSPECT F25 REPLACING ALL "/" BY "-".

           IF CK-BATCHID > 0
              MOVE CK-BATCHID    TO F26.
           MOVE CK-UNPAID        TO F27.
           MOVE CK-CLEARED-FG    TO F28.
           MOVE CK-CLEARED-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO F29.

           MOVE CK-TILLNO        TO F30.
           MOVE CK-EVENT-CD      TO F31.

           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF  TO WR-BUF.
           PERFORM CALL-WRFORM.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPCKCL.CPY".

      ******************************************************************
       FORM-RESET SECTION.
      ******************************************************************

           MOVE EXT-CONAME-CONAME   TO CKMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE  TO CKMAIN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CKMAIN-SCREEN UPON CKMAIN-WINDOW-HANDLE.
           MOVE CKMAIN-FORM-FIELDS TO WR-BUF.
           MOVE CKMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************************
           MOVE CKMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CKMAIN-HELP--DIR TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************************

           MOVE "00"                 TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.

           EVALUATE TRUE
                    COPY "LIBMN/CKMAIN_EVA.CPY".
                    WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       INVALID-CHECK-TYPE.
           MOVE "TYPE MUST BE  LN, DL, DR, DS, DP, MS, DD" TO MESS.
           PERFORM SEND-MESS.

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


      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPCK1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CKMAIN-SCREEN.
           DESTROY CKMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
