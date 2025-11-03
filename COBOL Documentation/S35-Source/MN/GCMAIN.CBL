       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GCMAIN.
      **************************************************************************
      *
      * IDENTIFICATION:  MN/GCMAIN.C
      *
      * DESCRIPTION   :  G/L FEE CODE FILE MAINTENANCE
      *-----------------------------------------------------------------
      * REV:
      * JTG 970814 ADDED LOGIC FOR GIFTNET FIELDS WORLD A90
      * GLF 970918 REPLACED ANY PROMPT THAT DISPLAYS WORD 'GIFTNET'
      *             W/'WCBC'
      * KEC 990225 ADDED 'EXITPROG' LOGIC.
      * BAH 030424 ADDED 8. ALTERNATE G/L ACCT NO: LENDMARK PR# 2048
      * JTG 030425 ADDED WCBC FEE GL ENTRY FILE                       WORLD #323
      * GLB 030513 UPDATED FROM A15
      * CS  031107 ADDED 9. COMMISSION G/L ACCT NO: (CD-GC-COMM-GLFEENO)
      * CS  031107 ADDED 10.COMMISSION PERCENT    : (CD-GC-COMM-PERCENT)
      * CS  031118 ADD CHECK IF CD-GC-COMM-PERCENT > 0
      *            MUST ENTER CD-GC-COMM-GLFEENO        [WORLD PR#331]
      * BAH 031211 WRITE CHANGES AND DELETE TO CHANGES LOG FILE
      * JTG 031218 ADDED LOGIC FOR GIFTNET TO VERIFY THAT CT-GLAMT(1)
      *            MATCHES CD-GC-FEE-PRICE    WORLD JIM R      J041
      *  CS 050408 FIXED ALTERNATE GL ACCTNO... WHEN COMMISSION GL ACCOUNT
      *            ADDED, ALLOWED FOR INPUT OF BRANCH.  IN DISPLAY FIELDS THE
      *            GL-BRANCH GOT CHANGED TO THE BRNO IN CD-GC-COMM-GLFEENO.  IN
      *            JIMS CASE IT WAS BRANCH 0, WHEN YOU ENTERED ALTERNATE GL
      *            ADDED MOVE OF BR-NO TO GL-BRANCH  PR#477 LENDMARK
      *  CS 050411 IF ALTERNATE GL IS 2020202 OR 0202020 MOVE GLFEENO TO
      *            ALTERNATE        PR#477
      * JTG 050608 ADDED CHANGES FOR REPORTING AND TAKING FEE CODE EARNINGS
      *            CREATED CD-GC-FEE-EARNINGS-FG         LENDMARK #2542
      *  CS 150305 CALL TO WI/CTENTR MISSING EXIT-PATH
      *  CS 150331 DUPLICATE GLSCAN IN WI/GL, CHANGED WI TO GLSCN2
      *  CS 150812 ADD CD-GC-BRANCH-COMM-AMOUNT, THIS AMOUNT WHEN ENTERED WILL
      *            REMAIN IN CD-GC-GLFEENO AND THE REMAINDER WILL GO TO
      *            CD-GC-COMM-GLFEENO.(OPPOSITE OF NORMAL FLOW) SO ITS TIED
      *            TO GP-I-AM-EXCEL  EXCEL PR#4821
      * KEC 2016.0316 CHANGED CD-FEE???? TO CD-GC-????.
      * KEC 2016.0325 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *               LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *               CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *               CDSCAN-??-KEY (NEW).
      * BAH 190130 NOW IN CDBFILE, REMOVED CTFILE 
      * BAH 2021.01.27 SOMEONE REMOVED THE SEND-GIFTNET-PROMPTS THAT WAS IN A15
      *                AND THIS WAS SETTING THE CHFG TO "N" FOR WCBC FIELDS
      *                5, 6 AND 7 SO I FORCED THEM TO "N" IN WORKING STORAGE
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
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)MN/GCMAIN S34 07/12/21-13:15:34 >".

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
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".

      *-----------------------------------------------------------------

       01  ERRCD                       PIC X         VALUE SPACES.
       01  HOLD-CDB1-KEY.
           03  HOLD-CDB-TYPE         PIC X(2)     VALUE SPACES.
           03  HOLD-CDB-BRANCH         PIC 9(4)     VALUE ZEROES.
           03  HOLD-CDB-CODE         PIC X(8)     VALUE SPACES.

       01  CHANGES-CDB-KEY.
           03  CHANGES-CDB-TYPE      PIC X(2).
           03  CHANGES-CDB-BRANCH      PIC 9(4).
           03  CHANGES-CDB-CODE      PIC X(8).

       01  CHANGED-CDB-REC          PIC X(CDB-SIZE).
       01  ORIGINAL-CDB-REC         PIC X(CDB-SIZE).

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  ACTION.
           03  CHKEY                   PIC X(04)     VALUE SPACES.
           03  CHNO                    PIC 9(02)     VALUE ZEROES.

       01  CHAN                    PIC X(04).

       01  ENTER-BR-FG                 PIC X(01)     VALUE "Y".
       01  FIRST-TIME                  PIC 9(01)     VALUE ZEROES.
       01  IO-TYPE                     PIC X(03)     VALUE SPACES.
       01  NEW-FLAG                    PIC 9(01)     VALUE ZEROES.
       01  VERIFY-STAT                 PIC X(2)      VALUE "00".

       01  SPLIT-GC-ACCT               PIC 9(11)     VALUE ZEROS.
       01  FILLER REDEFINES SPLIT-GC-ACCT.
           03  SPLIT-GC-BRANCH         PIC 9(4).
           03  SPLIT-GC-ACCOUNT        PIC 9(7).

       01  ONE                     PIC S9(1)     VALUE ZEROES.

       01  WS-CD-GC-COMM-GLFEENO       PIC 9(11)     VALUE ZEROS.
       01  WS-CD-GC-ALT-GLFEENO        PIC 9(07)     VALUE ZEROS.
       01  WS-CD-GC-BRANCH-COMM-AMOUNT PIC 9(6)      VALUE ZEROS.

      ******************************************************************
      *
      *    D I S P L A Y - W O R K E R S
      *
      ******************************************************************
       01  DISPLAY-BUF                               VALUE SPACES.
           03  D-GLNO                  PIC 9(07).
           03  F01                     PIC X(15).
           03  F02                     PIC X(01).
           03  F03                     PIC X(01).
           03  F04                     PIC ZZZZ9.99.
           03  F05                     PIC Z(05).
           03  F06                     PIC Z(03)9.99 BLANK ZEROES.
           03  F07                     PIC Z(05).
           03  F08                     PIC 9(07).
           03  F09                     PIC Z9.99.
           03  F10                     PIC ZZZ9.
           03  F101                    PIC 9(7).
           03  F11                     PIC X(01).
           03  F12                     PIC ZZZZZ9.
      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER PIC X(09) VALUE "GLACCTNO ".
           03  FILLER PIC X(28) VALUE "010 020 030 040 050 060 070 ".
           03  FILLER PIC X(24) VALUE "080 090 100 101 110 120.".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      ******************************************************************
       78  MAX                                       VALUE 12.

       77  ELE                         PIC 9(02)     VALUE ZEROES.

      *=================================================================
       01  SPEC-TABLE.
      * DESCRIPTION:
           03  FILLER PIC X(08) VALUE "E  A150 ".
      * FEE IS FINCHG:
           03  FILLER PIC X(08) VALUE "E  A010 ".
      * WCBC FLAG
           03  FILLER PIC X(08) VALUE "E  A010 ".
      * MAX FEE AMT
           03  FILLER PIC X(08) VALUE "ENNN052 ".
      * WCBC PRODUCT #
           03  FILLER PIC X(08) VALUE "ENNN050N".
      * WCBC PRICE:
           03  FILLER PIC X(08) VALUE "ENNN042N".
      * NEW GN PRODUCT #
           03  FILLER PIC X(08) VALUE "ENNN050N".
      * 8. ALTERNATE G/L ACCT NO
           03  FILLER PIC X(08) VALUE "ENNN070 ".
      * 9. COMMISSION PERCENT
           03  FILLER PIC X(08) VALUE "ENNN022 ".
      *10. COMMISSION G/L BRANCH
           03  FILLER PIC X(08) VALUE "ENNN040 ".
      *11. EARNINGS FEE CODE
           03  FILLER PIC X(08) VALUE "E  A010 ".

      *12. BRANCH OWNED COMMISSION
           03  FILLER PIC X(08) VALUE "ENNN060N".
      *-----------------------------------------------------------------
       01  SPEC-TABLE2                 REDEFINES SPEC-TABLE.
           03  SPEC-REC                OCCURS MAX TIMES.
               05  SPEC                PIC X(07).
               05  CHFG                PIC X(01).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/CDBSCNW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       01  GCMAIN-WINDOW-HANDLE  PIC X(10).
       COPY "LIBWI/GLSCN2W.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/GCMAIN_DEF.CPY".
       COPY "LIBMN/GCMAIN_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBMN/GCMAIN_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
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

      D     STOP MESS.

            PERFORM SQL-CONNECT.


       COPY "LIBGB/CHKSEC.CPY".

           MOVE "GCMAIN" TO VDU-FORM-NAME.
           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GCMAIN-WINDOW-HANDLE.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           PERFORM FORM-RESET.
       MAIN-MODULE.
           IF ( FIRST-TIME = 0 )
              MOVE 01  TO FIRST-TIME
              MOVE "Y" TO ENTER-BR-FG
            ELSE
              PERFORM FORM-RESET.

           PERFORM ENTER-CD-GC-KEY.

           IF ( IO-TYPE = "END" ) GO TO MAIN-PROGRAM-EXIT.
           IF ( IO-TYPE = "CLR" ) MOVE "Y" TO ENTER-BR-FG
                                  GO TO MAIN-MODULE.

       RETRY-MODULE.
           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM VERIFY-MODULE
              IF VERIFY-STAT = "00"
                 PERFORM WRITE-MODULE
             ELSE
                 GO TO RETRY-MODULE.


      *    IF ( IO-TYPE = "RWR" ) PERFORM WRITE-MODULE.


           IF ( IO-TYPE = "DEL" ) PERFORM DELETE-MODULE
                                  MOVE "N" TO ENTER-BR-FG.

           GO TO MAIN-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ********************************************
       VERIFY-MODULE SECTION.
      ********************************************
           MOVE "00" TO VERIFY-STAT.

      * IF COMMISSION > ZERO MUST ENTER GL NUMBER
           IF (CDB-GC-COMM-PERCENT > ZERO OR
               CDB-GC-BRANCH-COMM-AMOUNT > 0)
               IF CDB-GC-COMM-GLFEENO = ZEROS
                  MOVE "XX" TO VERIFY-STAT
                  MOVE "COMMISSION GL NUMBER CAN NOT BE ZEROS "
                                                        TO MESS
                  PERFORM SEND-MESS
                  GO TO VERIFY-MODULE-EXIT.

           IF CDB-GC-COMM-PERCENT NOT = ZEROS AND
              CDB-GC-BRANCH-COMM-AMOUNT NOT = ZEROS
              MOVE "XX" TO VERIFY-STAT
              MOVE "CANNOT USE BOTH COMM PERCENT AND COMM AMOUNT"
                                                   TO MESS
              PERFORM SEND-MESS
              GO TO VERIFY-MODULE-EXIT.

       VERIFY-MODULE-EXIT.
           EXIT.

      ********************************************
       DELETE-MODULE SECTION.
      ********************************************

           IF NEW-FLAG NOT = 0
              GO TO DELETE-MODULE-EXIT.

           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           IF ( IO-BAD )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO DELETE-MODULE-END.

           IF ( CDB-REC NOT = ORIGINAL-CDB-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO DELETE-MODULE-END.

           PERFORM DELETE-CDB1-FILE.
           PERFORM WRITE-CHANGES-FILE.

       DELETE-MODULE-END.

           PERFORM CLOSE-CDB1-FILE.

       DELETE-MODULE-EXIT.
           EXIT.

      ********************************************
       DISPLAY-FIELDS SECTION.
      ********************************************
           MOVE "N" TO CHFG(5) CHFG(6) CHFG(7).

           MOVE SPACES TO DISPLAY-BUF.

           MOVE CDB-GC-GLFEENO                 TO D-GLNO GL-ACCOUNT.
           PERFORM GET-GL.
           IF ( IO-BAD )
              MOVE "*** NOT FOUND ***"     TO GL-DESCRIPTION.
           PERFORM SEND-GL-DESC.

           MOVE CDB-DESC                    TO F01.
           MOVE CDB-GC-GLFEEFINCHG             TO F02.
           MOVE CDB-GC-FEE-GIFTNET             TO F03.
           MOVE CDB-GC-FEE-MAXAMT              TO F04.
           MOVE CDB-GC-FEE-PRODNO              TO F05.
           MOVE CDB-GC-FEE-PRICE               TO F06.
           MOVE CDB-GC-NEW-PRODNO              TO F07.

           MOVE CDB-GC-ALT-GLFEENO TO WS-CD-GC-ALT-GLFEENO.

           IF ( WS-CD-GC-ALT-GLFEENO     = 0202020) OR
              ( WS-CD-GC-ALT-GLFEENO     = 2020202)
              MOVE ZEROS      TO  CDB-GC-ALT-GLFEENO.

           MOVE CDB-GC-ALT-GLFEENO             TO F08 GL-ACCOUNT.
           PERFORM GET-GL.
           IF ( IO-BAD )
              MOVE "*** NOT FOUND ***"     TO GL-DESCRIPTION.

           MOVE "S  A320081." TO VDU.
           MOVE GL-DESCRIPTION   TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDB-GC-COMM-PERCENT           TO F09.
           MOVE CDB-GC-COMM-GLFEENO           TO SPLIT-GC-ACCT.
           MOVE SPLIT-GC-BRANCH           TO F10 GL-BRANCH.
           MOVE SPLIT-GC-ACCOUNT          TO F101 GL-ACCOUNT.
           PERFORM GET-GL.

           IF ( IO-BAD )
              MOVE "*** NOT FOUND ***"     TO GL-DESCRIPTION.
           MOVE "S  A320102." TO VDU.
           MOVE GL-DESCRIPTION   TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDB-GC-FEE-EARNINGS-FG        TO F11.
           MOVE CDB-GC-BRANCH-COMM-AMOUNT TO WS-CD-GC-BRANCH-COMM-AMOUNT.
           IF ( WS-CD-GC-BRANCH-COMM-AMOUNT = 202020)OR
              ( WS-CD-GC-BRANCH-COMM-AMOUNT = 020202)
              MOVE ZEROS      TO  CDB-GC-BRANCH-COMM-AMOUNT.

           MOVE CDB-GC-BRANCH-COMM-AMOUNT TO F12.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ********************************************
       ENTER-CD-GC-KEY SECTION.
      ********************************************

       ENTER-CD-GC-BRNO.

           MOVE 0   TO NEW-FLAG.
           MOVE " " TO IO-TYPE.

           IF ( ENTER-BR-FG = "N" )
              MOVE "SNNN040BRANCH." TO VDU
              MOVE BR-NO           TO VDUNUM0
                                       CDB-BRANCH
                                       GL-BRANCH
                                       HOLD-CDB-BRANCH
              PERFORM SCREENIO
              GO TO ENTER-READ-BRANCH.

           MOVE "EN N040BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F7-KEY ) MOVE "END" TO IO-TYPE
                                GO TO END-ENTER-CD-GC-KEY.
           IF ( NOT ENTER-KEY ) GO TO ENTER-CD-GC-BRNO.

           IF ( VDUNUM0 = ZEROES )
              GO TO ENTER-CD-GC-BRNO.

           MOVE VDUNUM0 TO CDB-BRANCH BR-NO BR-NO GL-BRANCH
                                             HOLD-CDB-BRANCH.

       ENTER-READ-BRANCH.

           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
              PERFORM NOT-FOUND
              MOVE "Y" TO ENTER-BR-FG
              GO TO ENTER-CD-GC-BRNO.

           MOVE "S  A040BRNAME." TO VDU.
           MOVE BR-NAME          TO VDUBUF.
           PERFORM SCREENIO.

       ENTER-CD-GC-FEECODE.

           MOVE SPACES TO IO-TYPE.

           MOVE "ER A020FEECD." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-CD-GC-BRNO.

           IF (        F1-KEY ) MOVE "CLR" TO IO-TYPE
                                GO TO END-ENTER-CD-GC-KEY.

           IF (        F6-KEY ) PERFORM SCAN-CD-FILE-GC.

           IF (        F7-KEY ) MOVE "END" TO IO-TYPE
                                GO TO END-ENTER-CD-GC-KEY.

           IF ( NOT ENTER-KEY ) GO TO ENTER-CD-GC-FEECODE.

           IF ( VDUBUF = SPACES )
              GO TO ENTER-CD-GC-FEECODE.

           MOVE VDUBUF TO CDB-CODE HOLD-CDB-CODE.

           MOVE "GC"   TO HOLD-CDB-TYPE CDB-TYPE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF IO-BAD
              PERFORM NEW-RECORD-SETUP
              GO TO ENTER-GLNO.

           MOVE CDB-REC TO ORIGINAL-CDB-REC.

           IF ( CDB-GC-COMM-PERCENT = 20.20 )
              MOVE ZEROES TO CDB-GC-COMM-PERCENT.

           MOVE CDB-GC-COMM-GLFEENO TO WS-CD-GC-COMM-GLFEENO.

           IF ( WS-CD-GC-COMM-GLFEENO     = 02020202020) OR
              ( WS-CD-GC-COMM-GLFEENO     = 20202020202)
              MOVE ZEROES TO  CDB-GC-COMM-GLFEENO.

           MOVE CDB-GC-ALT-GLFEENO TO WS-CD-GC-ALT-GLFEENO.

           IF ( WS-CD-GC-ALT-GLFEENO     = 02020202020) OR
              ( WS-CD-GC-ALT-GLFEENO     = 20202020202)
              MOVE ZEROES TO  CDB-GC-ALT-GLFEENO.

           MOVE CDB-GC-BRANCH-COMM-AMOUNT TO WS-CD-GC-BRANCH-COMM-AMOUNT.

           IF  (WS-CD-GC-BRANCH-COMM-AMOUNT = 020202) OR
               (WS-CD-GC-BRANCH-COMM-AMOUNT = 202020)
                MOVE ZEROES TO  CDB-GC-BRANCH-COMM-AMOUNT.

           PERFORM DISPLAY-FIELDS.

           GO TO END-ENTER-CD-GC-KEY.

       ENTER-GLNO.

           MOVE SPACES TO IO-TYPE.

           MOVE "EN N070GLACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-CD-GC-FEECODE.
           IF (        F1-KEY ) MOVE "CLR" TO IO-TYPE
                                GO TO END-ENTER-CD-GC-KEY.

           IF (        F6-KEY ) PERFORM SCAN-GL-FILE.
           IF ( NOT ENTER-KEY ) GO TO ENTER-GLNO.

           IF ( VDUNUM0 = ZEROES )
              GO TO ENTER-GLNO.

           MOVE VDUNUM0 TO CDB-GC-GLFEENO GL-ACCOUNT.

           PERFORM GET-GL.
           IF ( IO-BAD )
              PERFORM NOT-FOUND
              GO TO ENTER-GLNO.

           MOVE GL-DESCRIPTION TO CDB-DESC.
           PERFORM SEND-GL-DESC.
           PERFORM SEND-GLFEE-DESC.

           MOVE "SN N070080." TO VDU.
           MOVE CDB-GC-GLFEENO TO CDB-GC-ALT-GLFEENO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A320081." TO VDU.
           MOVE GL-DESCRIPTION   TO VDUBUF.
           PERFORM SCREENIO.

       END-ENTER-CD-GC-KEY.
           EXIT.

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

           MOVE SPACES TO IO-TYPE.
           MOVE 01     TO ONE.


       CREATE-ROUTINE.

           IF ( NEW-FLAG NOT = 1 )
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ( ELE     < 1   ) MOVE 1 TO ELE ONE.
           IF ( ELE NOT > MAX ) GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

       MODIFY-ROUTINE.

           MOVE "S  A300WSEL." TO VDU.
           MOVE "              FIELD #, DELETE:" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY        )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( NOT ENTER-KEY ) GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ACTION = " "
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.

           IF ( CHNO NOT NUMERIC ) GO TO MODIFY-ROUTINE.
           IF ( CHNO = ZEROES    ) GO TO MODIFY-ROUTINE.
           IF ( CHNO > MAX       ) GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

       ENTER-ELE.

           IF ( CHKEY NOT    = ZEROES ) AND
              ( CHFG(ELE)    = SPACES ) AND
              ( NEW-FLAG NOT = 1      )
              GO TO MODIFY-ROUTINE.

           IF ( CHFG(ELE) = "N" )
              IF ( NEW-FLAG = 1 )
                 GO TO CREATE-ROUTINE
              ELSE
              IF ( CHKEY NOT = CHAN )
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE ZEROES    TO VDUCOL.
           MOVE "."       TO VDUDECIMAL.
           PERFORM SCREENIO.

           IF ( UP-KEY )
              IF ( NEW-FLAG = 1 )
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.

           IF ( DOWN-KEY )
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

           IF ( F1-KEY )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( NOT ENTER-KEY )
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08
                 F-09 F-10 F-11 F-12
                 DEPENDING ON ELE.

      *=================================================================
      *
      *    SET RECORD ELEMENTS
      *
      *=================================================================
       F-01.

           MOVE VDUBUF TO CDB-DESC.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.

           MOVE VDUBUF TO CDB-GC-GLFEEFINCHG.

           IF NOT ( CDB-GC-GLFEEFINCHG = "Y" OR "N" )
              MOVE "ENTER 'N' OR 'Y'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.

           MOVE VDUBUF TO CDB-GC-FEE-GIFTNET.
           IF CDB-GC-FEE-GIFTNET NOT = "N"
              MOVE "ENTER 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.

           MOVE VDUNUM2 TO CDB-GC-FEE-MAXAMT.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.

           MOVE VDUNUM0 TO CDB-GC-FEE-PRODNO.

           IF CDB-GC-FEE-PRODNO = 0
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.

           MOVE VDUNUM2 TO CDB-GC-FEE-PRICE.

           IF ( CDB-GC-FEE-PRICE = ZEROES )
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.

           MOVE VDUNUM0 TO CDB-GC-NEW-PRODNO.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE BR-NO TO GL-BRANCH.
           MOVE VDUNUM0 TO CDB-GC-ALT-GLFEENO GL-ACCOUNT.


           PERFORM GET-GL.

           IF ( IO-BAD )
              PERFORM NOT-FOUND
              GO TO ENTER-ELE.

           MOVE "S  A320081." TO VDU.
           MOVE GL-DESCRIPTION   TO VDUBUF.
           PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE VDUNUM2 TO CDB-GC-COMM-PERCENT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------

       F-10.
           MOVE VDUNUM0 TO GL-BRANCH SPLIT-GC-BRANCH.

       F-101.
           MOVE "ENNN070101." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUNUM0 TO GL-ACCOUNT SPLIT-GC-ACCOUNT.

           IF NOT ( SPLIT-GC-ACCOUNT = ZEROS)
               PERFORM GET-GL
               IF IO-BAD
                 PERFORM NOT-FOUND
                 GO TO ENTER-ELE.

           MOVE SPLIT-GC-ACCT    TO CDB-GC-COMM-GLFEENO.

       F-102.
           MOVE "S  A320102." TO VDU.
           IF SPLIT-GC-ACCOUNT = ZEROS
              MOVE SPACES TO VDUBUF
           ELSE
              MOVE GL-DESCRIPTION   TO VDUBUF.
           PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.

       F-11.
           MOVE VDUBUF TO CDB-GC-FEE-EARNINGS-FG.
           IF NOT (CDB-GC-FEE-EARNINGS-FG = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-12.
           MOVE VDUNUM0 TO CDB-GC-BRANCH-COMM-AMOUNT.
             GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ********************************************
       NEW-RECORD-SETUP SECTION.
      ********************************************

           MOVE 1 TO NEW-FLAG.

           MOVE "S  A000NEW."        TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-CDB-FILE.

           MOVE HOLD-CDB1-KEY  TO CDB1-KEY.

           MOVE "SNNN052040."        TO VDU.
           MOVE 99999.99             TO CDB-GC-FEE-MAXAMT VDUNUM2.
           PERFORM SCREENIO.

           MOVE ZEROES         TO ELE.

       NEW-RECORD-SETUP-EXIT.
           EXIT.

      ********************************************
       SCAN-CD-FILE-GC SECTION.
      ********************************************

           MOVE "GC"       TO CDBSCN-TYPE.
           MOVE BR-NO     TO CDBSCN-BRANCH.
           MOVE SPACES     TO CDBSCN-CODE.

           MOVE "WI/CDBSCN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDBSCN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDBSCN-STATUS NOT = "00") OR (CDBSCN-CODE = SPACES) 
              MOVE "XX" TO VDUSTATUS
              GO TO SCAN-CD-FILE-GC-EXIT.

           MOVE "S  A020FEECD." TO VDU.
           MOVE CDBSCN-CODE     TO VDUBUF.
           PERFORM SCREENIO.

       SCAN-CD-FILE-GC-EXIT.
           EXIT.

      ********************************************
       SCAN-GL-FILE SECTION.
      ********************************************

           MOVE SPACES     TO GLSCN2-KEY.

           MOVE "WI/GLSCN2"               TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GLSCN2-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF GLSCN2-KEY  = SPACES 
              MOVE "XX" TO VDUSTATUS
              GO TO SCAN-GL-FILE-EXIT.

           MOVE "SNNN070GLACCTNO." TO VDU.
           MOVE GLSCN2-ACCOUNT     TO VDUNUM0.
           PERFORM SCREENIO.

       SCAN-GL-FILE-EXIT.
           EXIT.

      ********************************************
       WRITE-MODULE SECTION.
      ********************************************

           PERFORM OPEN-CDB1-FILE.

           IF ( NEW-FLAG NOT = ZEROES )
              GO TO WRITE-RECORD.

       REWRITE-RECORD.

           MOVE CDB-REC TO CHANGED-CDB-REC.

           IF ( ORIGINAL-CDB-REC = CHANGED-CDB-REC )
              GO TO WRITE-MODULE-END.

           PERFORM READ-CDB1-FILE.
           IF ( IO-BAD )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO WRITE-MODULE-END.

           IF ( CDB-REC NOT = ORIGINAL-CDB-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO WRITE-MODULE-END.

           MOVE CHANGED-CDB-REC TO CDB-REC.
           PERFORM REWRITE-CDB1-FILE.
           PERFORM WRITE-CHANGES-FILE.

           GO TO WRITE-MODULE-END.

       WRITE-RECORD.

           PERFORM WRITE-CDB1-FILE.
           IF IO-BAD
              PERFORM ALREADY-CREATED.

       WRITE-MODULE-END.

           PERFORM CLOSE-CDB1-FILE.

       WRITE-MODULE-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPCDBCL.CPY".
       COPY "LIBGB/DATER.CPY".

      ********************************************************
       FORM-RESET SECTION.
      ********************************************************
           MOVE EXT-CONAME-CONAME TO GCMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GCMAIN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY GCMAIN-SCREEN UPON GCMAIN-WINDOW-HANDLE.
           MOVE GCMAIN-FORM-FIELDS TO WR-BUF.
           MOVE GCMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ********************************************************
       SETUP-LINK-INFO SECTION.
      ********************************************************
           MOVE GCMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE GCMAIN-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/GCMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************
       MISC-ROUTINES SECTION.
      ************************
       COPY "LIBGB/ACCESS.CPY".
      *-----------------------------------------------------------------
       WRITE-CHANGES-FILE.
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE CDB-TYPE    TO CHANGES-CDB-TYPE.
           MOVE CDB-BRANCH  TO CHANGES-CDB-BRANCH.
           MOVE CDB-CODE    TO CHANGES-CDB-CODE.
           MOVE CHANGES-CDB-KEY TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME   TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.
      *-----------------------
       GET-GL.
           PERFORM OPEN-GL1-FILE.
           PERFORM READ-GL1-FILE.
           PERFORM CLOSE-GL1-FILE.

      *-----------------------------------------------------------------
       SEND-GL-DESC.

           MOVE "S  A320GLDESC." TO VDU.
           MOVE GL-DESCRIPTION   TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       SEND-GLFEE-DESC.

           MOVE "S  A150010." TO VDU.
           MOVE CDB-DESC      TO VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ACTIVE-RECORD.

           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ALREADY-CREATED.

           MOVE "CD-RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       CHANGE-AGAIN.

           MOVE " PLEASE RE-INPUT CD-RECORD CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       NOT-FOUND.
           MOVE " NOT FOUND" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       RECORD-CHANGED.

           MOVE " CHANGES WERE MADE TO THIS CD-RECORD!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       RECORD-DELETED.

           MOVE " CD-RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       UNDERGONE-CHANGE.

           MOVE " THIS CD-RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
             TO MESS.

           PERFORM SEND-MESS.

      ****************************************
       IO-ROUTINES SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBLP/LPCDB1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ****************************************
       END-PROGRAM SECTION.
      ****************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.

           MOVE EXIT-PATH  TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GCMAIN-SCREEN.
           DESTROY GCMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
