       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNMAN1.
      ******************************************************************
      *
      *   DIR :  MN
      *   DESC: LOAN MASTER FILE MAINTENANCE
      *
      *   HOW TO DELETE A VOIDED LOAN (I ALWAYS FORGET HOW TO DO THIS!):
      *      CALL UP THE ACCT
      *      IT SAYS "ACCOUNT HAS BEEN DELETED", JUST PRESS ENTER
      *      FIELDS WON'T DISPLAY, TYPE 3535 & RETURN, THEY WILL DISPLAY
      *      CHANGE POFF CODE TO SPACES (DAILY PASSWORD FOR FIELD 26)
      *      HIT RETURN TO ACCEPT RECORD, THEN CALL IT BACK UP
      *      THE BALANCE ON P. 5 IS 0, CHANGE TO BE SAME AS LOAN AMT
      *      CHANGE THE PAYOFF DATE ON PAGE 2 TO 0
      *      BACK ON PAGE 1 AT SELECTION FIELD, TYPE 9898 & RETURN
      *      BACK ON PAGE 1 AT SELECTION FIELD, TYPE "DELETE" & RETURN
      *                                                   BARB
      *
      *=================================================================
      * REV:
      * MJD 050895 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      * JTG 110195 ADDED OPTION TO RECALL DELETED LOANS
      * JTG 081496 INSURED THAT ALL SOC-SEC-NO'S IN LOAN RECORD
      *            WERE UNIQUE
      * JFF 101196 CHANGED #1-5 ENTRY LINES TO GROUP SPR KEY
      *            TOGETHER
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * MJD 071597 CHANGED LN-CAN-LOAN-NO TO PIC XX.
      * JTG 081497 ADDED SET OF LN-GIFTNET WORLD A90
      * MJD 082197 CHANGED RECORD SIZE FROM 1500 TO 1570
      *            ADD LOGIC FOR PAGE 10.
      * MJD 112197 EXPANDED LN-REC TO 1800 BYTES
      * JTG 060398 ADDED LN-PO-TRCD TO HOLD THE TRCD USED TO PAYOFF THIS LOAN
      * JTG 082698 ADDED DISPLAY OF LN-GIFTNET CONDITIONED ON GP-I-AM-WORLD
      * KEC 090398 ADDED DISPLAY OF LN-REVOLVING.
      * KEC 091598 ADDED LOGIC TO ACCESS NEW PAGE 11.
      * KEC 091598 ADDED LOGIC TO ACCESS NEW PAGE 12.
      * MJD 990202 ADDED LN-ACCT-FROZEN
      * GLF 990920 CHANGE SSNO ENTRY TO USE NEW VDUCLASS "X" EDIT, MAKE SSNO
      *            ENTRY CONSISTENT THROUGHOUT
      *  CS 991011 ADDED "B" TO VALID LN-CONVERCD
      * MJD 991223 MOVED SEND-MESS LOGIC THAT OCCURED AT END OF DISPLAY-FIELDS
      *           TO WHERE PROCEDURE IS CALLED.  DISPLAY FIELDS IS PERFORMED
      *           FROM WITHIN THE LOAN SCAN AND WE DO NOT WANT THE SEND
      *           MESS TO OCCUR THERE.
      * BAH 000502 CHANGED MODIFY-BORROWER SECTION, WAS NOT LETTING YOU ZERO
      *            OUT THE SSNO TO REMOVE A CO-MAKER, FROM GLF SSNO ENTRY
      *            CHANGE ON 990920
      * MJD 000911 ADDED LN-LBOX (MFC PR#227)
      * KEC 010622 {DR#00002} PARADATA FINANCIAL [KEN]
      *            DUE TO CUSTOMERS RESCINDING (DELETE) A LOAN INSTEAD
      *            OF UPDATING IT, ALLOWED THE ENTRY OF "*P" IN THE
      *            LN-POCD (#26).
      * CS  010802 MUST ENTER 9898 THEN DELETE TO DELETE LOAN MASTER FILE
      *            LOGGED THE DELETE OF LOAN FROM LOAN MASTER FILE, PL# 309
      * BAH 020625 ADDED FIELD 24, LN-PLREASON, REGACC PR# 1775
      * BAH 030213 ADDED INTERIM TRW NEXT TO SKIP TRW FOR REGACC PR# 16
      * BAH 030605 ADDED LN-SKIP-TRW VALUE OF "E" TO SKIP REPORTING THE
      *            ENDORSER, JUST REPORT MAKER AND JOINT, ACC PR# 2081
      * BAH 050428 ADDED LN-ACH ON LINE 19 BY ALLOTMENT AND LBOX, REGACC PR#102
      * JTG 050922 ADDED LN-POCD-UNWIND "*U" A LN-POCD-NONLOAN
      *                  AND LN-UNWIND-REASON-CODE                 REGACC #0105
      * BLF 051011 ADDED DETAILED, FIELD-BY-FIELD MAINTENANCE LOG IN
      *            /USR/DATA/R1/LG/CHG.YYMMDD
      * JTG 051018 STOPPED CHANGES FOR LOANS FLAGGED AS LN-POCD-UNWIND
      *            LIKE LOANS WITH LN-POCD-VOID                    REGACC #0105
      * BAH 051027 ADDED LN-FROZEN-REASON, REGACC PR# 114
      * MJD 121017 A30 CONVERSION COMPLETE.
      * MJD 121217 ADDED LNMAIN-WINDOW-HANDLE TO SCREEN DISPLAY
      * MJD 130115 REMOVED UNUSED       LINES CLEANING UP
      *  CS 160711 OWNING BRANCH NEEDS REMOVED FROM ENTRY-MODULE SINCE
      *            WE DONT WANT USER TO CHANGE.
      * KEC 2017.0526 REMOVED ORIGINAL-REC AND CHANGED-REC WHICH NOW USING
      *               LNOR-REC AND LNCH-REC.
      * KEC 2017.0612 ADDED WARNING MESSAGE FOR CHANGE OF LN-OWNBR.
      * BAH 2018.1212 GLOBAL LNFILE
      * BAH 2019.0307 REMOVED LN-FROZEN-REASON1, USE A WORKING STORAGE FIELD
      *               INSTEAD. THAT FIELD REPLACED A COMP-3 OLD FIELD AND
      *               NEEDS TESTED FOR A X'00'
      * BAH 2019.0625 REMOVED LC-REVOLVING, PT#0153
      * BAH 2024.0130 CALL CRNOR2 AND WRITE NOTICE RECORD
      * BAH 2024.0610 CRNOR2 HANGING ON A NEW WRITE.  WE DONT HAVE ENOUGH
      *               VALID DATA YET TO PERFORM CREATE NOTICE, SO CLEAR 
      *               NOTICE REC AND WRITE NOTICE REC, ALSO DO NOT ALLOW
      *               A 0 IN BORROWER # 1, S35Q-65
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)MN/LNMAN1 S35 06/10/24-15:39:46 >".

      *#################################################################
      *
      *    THIS IS HERE FOR IF ANYONE WANTED TO KNOW HOW A '66' WORKS!!!
      *
      *#################################################################
      *01  WS-LN-REC.
      *    03  WS-LN-ACCOUNT-NUMBER.
      *        05  WS-NUMBER        PIC 9(06)     VALUE ZEROES.
      *        05  WS-SEQNO         PIC 9(02)     VALUE ZEROES.
      *    03  WS-LN-ACCTNO            REDEFINES WS-LN-ACCOUNT-NUMBER
      *                                PIC 9(08).
      *
      *01  WS-LN-REC.
      *    03  WS-LN-ACCOUNT-NUMBER.
      *        05  WS-NUMBER        PIC 9(06)     VALUE ZEROES.
      *    03  WS-LN-ACCOUNT-SEQNO.
      *        05  WS-LN-SEQNO         PIC 9(02)     VALUE ZEROES.
      *    66  WS-LN-ACCTNO            RENAMES WS-LN-ACCOUNT-NUMBER
      *                                   THRU WS-LN-ACCOUNT-SEQNO.
      *
      *#################################################################

      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".

      *=================================================================
      *   LNCH-REC FROM 'REPLACING LEADING' ON LN-REC. [CHANGED-REC]
      *   LNOR-REC FROM 'REPLACING LEADING' ON LN-REC. [ORIGINAL-REC]
      *=================================================================
       COPY "LIBLP/LP01LN.CPY" REPLACING LEADING "LN" BY "LNCH".
       COPY "LIBLP/LP01LN.CPY" REPLACING LEADING "LN" BY "LNOR".

      *=================================================================
       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR           PIC 9(4).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-NUMBER       PIC Z(7).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(10).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(12).
               05  WORK-ERRLOG-ORG-N0 REDEFINES WORK-ERRLOG-ORG-ALP
                                        PIC Z(11)-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES WORK-ERRLOG-ORG-ALP 
                                        PIC Z(8).99-.
               05  WORK-ERRLOG-TO-X     PIC X(3).
               05  WORK-ERRLOG-CHG-ALP  PIC X(12).
               05  WORK-ERRLOG-CHG-N0 REDEFINES WORK-ERRLOG-CHG-ALP
                                        PIC Z(11)-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES WORK-ERRLOG-CHG-ALP
                                        PIC Z(8).99-.

       01  ONE               PIC S9.
       01  SUB               PIC 99     COMP-3 VALUE 0.
       01  SUBX              PIC 99     COMP-3 VALUE 0.
       01  SUBY              PIC 99     COMP-3 VALUE 0.
       01  ELE               PIC 99     VALUE 0.
       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X REDEFINES CHNO
                             PIC X(2).
       01  FORM-NUM1     REDEFINES ACTION.
           03  FORM-KEY1     PIC X(4).
           03  FORM-P1       PIC X.
           03  FORM-SEL1     PIC 9.
       01  FORM-NUM2      REDEFINES ACTION.
           03  FORM-KEY2     PIC X(3).
           03  FORM-P2       PIC X.
           03  FORM-SEL2     PIC 99.
               88  FORM-2-DIGITS  VALUE 10 11.

       01  VOIDED            PIC X      VALUE " ".
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
      *01  FIRST-TIME        PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.
       01  X-ST-BR         VALUE SPACES.
           03  X-STATE     PIC XX.
           03  X-HYPH      PIC X.
           03  X-BR        PIC 9(4).
       01  TEST-FROZEN-REASON.
           03  TEST-FROZEN-REASON1 PIC X.
           03  FILLER              PIC X.
       01  TEST-POCD       PIC XX.
       01  FILLER REDEFINES TEST-POCD.
           03  TEST-POCD1  PIC X.
           03  TEST-POCD2  PIC X.

       01  CHAN          PIC X(4).

       01  HOLD-LN1-KEY.
           03  HOLD-LN-OWNBR   PIC 9(4).
           03  HOLD-LN-ACCTNO  PIC 9(8).

       01  INDX            PIC 9.
       01  VDU-INDX        PIC 99.
       01  BW-SKIP         PIC X.
       01  WK-MAKERCD      PIC X VALUE " ".
           88  GOOD-MAKERCD      VALUE "M" "J" "E".

      * 'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
      
       01  PAGE-CHANGE-FLAG      PIC X(1)    VALUE "N".
           88  PAGE-CHANGE-NO                VALUE "N".
           88  PAGE-CHANGE-YES               VALUE "Y".

       01  LNMAIN-LINK       VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(08).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  LNMAIN-WINDOW-HANDLE  PIC X(10).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF       VALUE SPACES.
           03  F01           PIC Z9.
           03  F02           PIC X(7).
           03  F03           PIC Z9.
           03  F04           PIC Z9.
           03  F05           PIC Z9.
           03  F06           PIC X.
           03  F07           PIC X(11).
           03  F071          PIC X.
           03  F072          PIC X(20).
           03  F08           PIC X(11).
           03  F081          PIC X.
           03  F082          PIC X(20).
           03  F09           PIC X(11).
           03  F091          PIC X.
           03  F092          PIC X(20).
           03  F10           PIC X(11).
           03  F101          PIC X(1).
           03  F102          PIC X(20).
           03  F11           PIC X(10).
           03  F12           PIC X.
           03  F13           PIC X(7).
           03  F131          PIC X.
           03  F14           PIC XXX.
           03  F15           PIC XX.
           03  F16           PIC X(20).
           03  F17           PIC XX.
           03  F18           PIC X.
           03  F19           PIC X.
           03  F191          PIC X.
           03  F192          PIC X.
           03  F20           PIC X.
           03  F21           PIC X.
           03  F22           PIC X.
           03  F221          PIC X.
           03  F23           PIC X.
           03  F24           PIC X.
           03  F241          PIC X.
           03  F242          PIC XX.
           03  F25           PIC X.
           03  F26           PIC XX.
           03  F261          PIC XX.
           03  F262          PIC XX.
           03  F27           PIC X.
           03  F28           PIC X.
           03  F29           PIC XX.
           03  F30           PIC X.
           03  F301          PIC XX.
           03  FLAGS.
               05  FSMALLOAN  PIC X.
               05  FSALEFIN   PIC X.
               05  FSERVACCT  PIC X.
               05  FLOANTYPE  PIC X.
               05  FGIFTNET   PIC X.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(24) VALUE
           "010 020 030 040 050 060 ".
           03  FILLER      PIC X(48) VALUE
           "070 071 072 080 081 082 090 091 092 100 101 102 ".
           03  FILLER      PIC X(48) VALUE
           "110 120 130 131 140 150 160 170 180 190 191 192 ".
           03  FILLER      PIC X(56) VALUE
           "200 210 220 221 230 240 241 242 250 260 261 262 270 280 ".
           03  FILLER      PIC X(18) VALUE
           "290 300 301 FLAGS.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       78  MAX                      VALUE 30.
       01  SPEC-TABLE.
      * 01.CLASS
           03  FILLER      PIC X(8) VALUE "EN N020N".
      * 02.ST/BRANCH
           03  FILLER      PIC X(8) VALUE "E  A050N".
      * 03.SPR CLASS
           03  FILLER      PIC X(8) VALUE "EN N020N".
      * 04.SUB CLASS
           03  FILLER      PIC X(8) VALUE "EN N020N".
      * 05.LAW CODE
           03  FILLER      PIC X(8) VALUE "E  N020N".
      * 06.MORE LOANS
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 07.BORR #1
           03  FILLER      PIC X(8) VALUE "ERNX090N".
      * 08.BORR #2
           03  FILLER      PIC X(8) VALUE "ERNX090N".
      * 09.BORR #3
           03  FILLER      PIC X(8) VALUE "ERNX090N".
      * 10.BORR #4
           03  FILLER      PIC X(8) VALUE "ERNX090N".
      * 11.STATUS CHANGE
           03  FILLER      PIC X(8) VALUE "E  A100N".
      * 12.LAST STATCHG
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 13.OWN ST/BR
           03  FILLER      PIC X(8) VALUE "E  A050N".
      * 14.PURPOSE CD
           03  FILLER      PIC X(8) VALUE "E  A030N".
      * 15.COLL CD
           03  FILLER      PIC X(8) VALUE "E  A020N".
      * 16.COLL ID
           03  FILLER      PIC X(8) VALUE "E  A200N".
      * 17.SOURCE CD
           03  FILLER      PIC X(8) VALUE "E  A020N".
      * 18.CONFID
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 19.ALLOT PLAN
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 20.CONV ACCT
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 21.MIDSTREAM PURCH
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 22.SKIP TRW
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 23.RECAST CD
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 24.P&L CD
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 25.REPO CD
           03  FILLER      PIC X(8) VALUE "E  A020N".
      * 26.PAYOFF CD
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 27.ADDON ACCT
      *--->03  FILLER      PIC X(8) VALUE "ENNN020N".
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 28.PAYMNT SCHD
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 29.CAN LOAN NO.
           03  FILLER      PIC X(8) VALUE "E  A020N".
      * 30.ACCOUNT IS FROZEN
           03  FILLER      PIC X(8) VALUE "E  A010N".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS MAX TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/LNMAN1_DEF.CPY".
       COPY "LIBMN/LNMAN1_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/LNMAN1_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ********************************************
       MAIN-PROGRAM SECTION.
      ********************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LNMAN1" TO VDU-FORM-NAME.
       COPY "LIBGB/CHKSEC.CPY".

           MOVE " " TO VDU-PGMENU-SELECTION.
           PERFORM OPEN-BW1-FILE.

           MOVE FORM-LINK-AREA TO LNMAIN-LINK.

       MAIN-MODULE.

           PERFORM FORM-RESET.

           PERFORM REC-LN-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.

           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P1
              MOVE 2 TO FORM-SEL1.
           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE LN-ACCTNO TO SAVEKEY
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.
           IF FORM-SEL2 NOT = 0
              IF FORM-P2 = "P"
                 MOVE LN-ACCTNO TO SAVEKEY
                 MOVE FORM-SEL2 TO FORMSEL
                 GO TO END-ROUTINE.
           MOVE "N" TO NEW-REC.
           GO TO MAIN-MODULE.

      ********************************************
       REC-LN-MODULE SECTION.
      ********************************************
           MOVE 0 TO LN-ACCTNO LN-OWNBR FIRST-SCAN.
           MOVE " " TO IO-TYPE VOIDED.
           IF SAVEKEY NOT = " "
              MOVE "Y" TO PAGE-CHANGE-FLAG
              MOVE SAVEKEY TO LN-ACCTNO
              MOVE SPACES TO SAVEKEY
              PERFORM DISPLAY-SCAN-KEY
              GO TO READ-LNFILE.

           MOVE "N" TO PAGE-CHANGE-FLAG.
           MOVE 0   TO NEW-FLAG.

       REC-LN-ACCTNO.
           MOVE "EN N060LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-LN-MODULE.
           IF VDUSTATUS = "1<"
             PERFORM ENTER-SCANKEY
             GO TO END-REC-LN-MODULE.
           IF VDUSTATUS NOT = "00"
             GO TO REC-LN-ACCTNO.
           MOVE VDUNUM0 TO LN-ACCTNO.
           IF LN-ACCTNO = 0
              GO TO REC-LN-ACCTNO.

           MOVE LN-ACCTNO TO HOLD-LN-ACCTNO.
           
       READ-LNFILE.
           PERFORM OPEN-LN1-FILE.
           MOVE LN-PATH-OWNBR TO HOLD-LN-OWNBR.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 0
              IF ( LN-POCD-VOID    ) OR
                 ( LN-POCD-RESCIND ) OR
                 ( LN-POCD-UNWIND  )
                 IF ( LN-POCD-VOID )
                    MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
                    PERFORM SEND-MESS
                 END-IF
                 IF ( LN-POCD-RESCIND )
                    MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
                    PERFORM SEND-MESS
                 END-IF
                 IF ( LN-POCD-UNWIND )
                    MOVE "ACCOUNT HAS BEEN UNWOUND " TO MESS
                    PERFORM SEND-MESS
                 END-IF
                 MOVE "R RA060SEL." TO VDU
                 PERFORM SCREENIO
                 IF VDUBUF = "  3535"
                    MOVE "Y" TO VOIDED
                    MOVE LN-REC TO LNOR-REC
                    PERFORM DISPLAY-FIELDS
                    PERFORM TEST-RESCISSION-STATUS
                    MOVE 0 TO NEW-FLAG
                    GO TO END-REC-LN-MODULE
                 ELSE
                    GO TO REC-LN-MODULE
              ELSE
                 MOVE LN-REC TO LNOR-REC
                 PERFORM DISPLAY-FIELDS
                 PERFORM TEST-RESCISSION-STATUS
                 MOVE 0 TO NEW-FLAG
                 GO TO END-REC-LN-MODULE.

           PERFORM NEW-RECORD-SETUP.

       END-REC-LN-MODULE.

      ********************************************
       ENTER-SCANKEY SECTION.
      ********************************************
           MOVE " ENTER LOAN #, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-LN1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.

           PERFORM START-LN1-FILE.
       ENTER-KEY.
           MOVE "EN N080SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUNUM0 = 0
              IF FIRST-SCAN NOT = 0
                 MOVE LN1-KEY TO HOLD-LN1-KEY
                 MOVE LN-REC TO LNOR-REC
                 PERFORM TEST-RESCISSION-STATUS
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE VDUNUM0       TO LN-ACCTNO.

           PERFORM START-LN1-FILE.
       READ-FILES-NEXT.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           IF ( LN-POCD-VOID ) OR ( LN-POCD-RESCIND )
              GO TO READ-FILES-NEXT.

      *    UNLOCK LN-FILE.
           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       DISPLAY-SCAN-KEY.
           MOVE "SN N060LNNO." TO VDU.
           MOVE LN-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.
       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           PERFORM CLOSE-LN1-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       END-SCANKEY.
           EXIT.

      ************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
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
              GO TO CHECK-PAGE.

      * MUST ENTER 9898 BEFORE DELETE IS ALLOWED
           IF ACTION = "  9898"
              MOVE "R RA060SEL." TO VDU
              PERFORM SCREENIO
              MOVE VDUBUF TO ACTION
              IF ACTION = "DELETE"
                 PERFORM VERIFY-DELETE
                 IF IO-FG = 9
                    GO TO MODIFY-ROUTINE
                 ELSE
                    MOVE "DEL" TO IO-TYPE
                    GO TO END-ENTRY-MODULE.

      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF VOIDED = " "
              IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
                 IF FORM-SEL1 < 2 OR FORM-SEL1 > 9
                    GO TO MODIFY-ROUTINE
                 ELSE
                    GO TO CHECK-PAGE.
      * FORM-NUM2 CHECKS FOR PAGES 10 THRU 99
           IF VOIDED = " "
              IF FORM-KEY2 = " " AND FORM-P2 = "P" AND FORM-SEL2 NUMERIC
                 IF NOT FORM-2-DIGITS
                    GO TO MODIFY-ROUTINE
                 ELSE
                    GO TO CHECK-PAGE.
           GO TO CHECK-CHNO.
       CHECK-PAGE.
           PERFORM TEST-SPR.
           IF IO-FG = 9
              PERFORM INVALID-SPR
              IF LN-CONVERCD = " "
                 GO TO MODIFY-ROUTINE.
           IF NEW-FLAG = 1 OR 2
              MOVE SP-EARN-ORDER(1) TO LN-EARN-ORDER(1)
              MOVE SP-EARN-ORDER(2) TO LN-EARN-ORDER(2)
              MOVE SP-EARN-ORDER(3) TO LN-EARN-ORDER(3)
              MOVE SP-EARN-ORDER(4) TO LN-EARN-ORDER(4)
              MOVE SP-EARN-ORDER(5) TO LN-EARN-ORDER(5)
              MOVE SP-EARN-ORDER(6) TO LN-EARN-ORDER(6).
           MOVE "RWR" TO IO-TYPE.
           GO TO EXIT-ENTRY-MODULE.
       CHECK-CHNO.
           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
           IF CHNO-X NOT NUMERIC
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
      * DONT ALLOW ENTRY TO LN-ADDON-FG UNLESS "++++27"
           IF ELE = 27
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 IF CHKEY NOT = "++++"
                    GO TO MODIFY-ROUTINE.
           IF ELE NOT = 27
              IF NEW-FLAG = 0
                 IF CHFG(ELE) = "N"
                    IF CHKEY NOT = CHAN
                       GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
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
           GO TO A-01 A-02 A-03 A-04 A-05 A-06 A-07 A-08
                 A-09 A-10 A-11 A-12 A-13 A-14 A-15 A-16
                 A-17 A-18 A-19 A-20 A-21 A-22 A-23 A-24
                 A-25 A-26 A-27 A-28 A-29 A-30
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *=================================================================
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *=================================================================
       A-01.
           MOVE VDUNUM0 TO LC-CODE
                           LN-CLASS.

           PERFORM VALIDATE-LN-CLASS.

           IF ( IO-FG NOT = ZEROES )
              GO TO ENTER-ELE.

           IF ( NEW-FLAG NOT = 1 )
              IF ( LC-LOANTYPE  NOT = LN-LOANTYPE  ) OR
                 ( LC-SALEFIN   NOT = LN-SALEFIN   ) OR
                 ( LC-SERVACCT  NOT = LN-SERVACCT  )
                 MOVE "CLASSES ARE NOT CONSISTENT IN TYPES" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              END-IF
           ELSE
              MOVE LC-SMALLOAN  TO LN-SMALLOAN
              MOVE LC-SALEFIN   TO LN-SALEFIN
              MOVE LC-SERVACCT  TO LN-SERVACCT
              MOVE LC-LOANTYPE  TO LN-LOANTYPE
              MOVE LC-GIFTNET   TO LN-GIFTNET.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       A-02.
           MOVE VDUBUF TO X-ST-BR.
           IF X-BR NOT NUMERIC
              GO TO ENTER-ELE.
           MOVE X-STATE TO ST1-KEY.
           PERFORM GET-ST1-FILE.
           IF IO-FG NOT = 0
              PERFORM INVALID-STATE
              GO TO ENTER-ELE.
           MOVE X-BR TO BR-NO.
           PERFORM GET-BR-FILE.
           IF IO-FG NOT = 0
              PERFORM INVALID-BRANCH
              GO TO ENTER-ELE.

           MOVE X-BR TO LN-ORGBR.
           MOVE X-STATE TO LN-ORGST.
           MOVE "-" TO X-HYPH.
           MOVE "S  A070020." TO VDU.
           MOVE X-ST-BR TO VDUBUF.
           PERFORM SCREENIO.
           IF NEW-FLAG NOT = 1
              PERFORM TEST-SPR
              IF IO-FG = 9
                 PERFORM INVALID-SPR.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       A-03.
           MOVE VDUNUM0 TO LN-SPRCLASS
                           LC-CODE.

           PERFORM VALIDATE-LN-CLASS.

           IF ( IO-FG NOT = ZEROES )
              MOVE "INVALID CLASS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF ( LC-LOANTYPE  NOT = LN-LOANTYPE  ) OR
              ( LC-SALEFIN   NOT = LN-SALEFIN   ) OR
              ( LC-SERVACCT  NOT = LN-SERVACCT  )
              MOVE "CLASSES ARE NOT CONSISTENT IN TYPES" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF ( NEW-FLAG NOT = 1 )
              PERFORM TEST-SPR
              IF ( IO-FG NOT = ZEROES )
                 PERFORM INVALID-SPR.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       A-04.
           MOVE VDUNUM0 TO LN-SUBCLASS.
           IF NEW-FLAG NOT = 1
              PERFORM TEST-SPR
              IF IO-FG = 9
                 PERFORM INVALID-SPR.
           GO TO CREATE-ROUTINE.
       A-05.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.
           MOVE VDUNUM0 TO LN-LAWCODE.
           PERFORM TEST-SPR.
           IF IO-FG = 9
              PERFORM INVALID-SPR.
           GO TO CREATE-ROUTINE.
       A-06.
      *    IF NOT (VDUBUF = "Y" OR " ")
      *       MOVE " ENTER: ' ' OR 'Y'" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO ENTER-ELE.
      *    MOVE VDUBUF TO LN-MORELN.
           GO TO CREATE-ROUTINE.
       A-07.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.
           MOVE 1 TO INDX.
           PERFORM MODIFY-BORROWER.
           IF BW-SKIP = "X"
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE 2 TO INDX.
           PERFORM MODIFY-BORROWER.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE 3 TO INDX.
           PERFORM MODIFY-BORROWER.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE 4 TO INDX.
           PERFORM MODIFY-BORROWER.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDUBUF TO LN-STATFLAGS.
           GO TO CREATE-ROUTINE.
       A-12.
           IF VDUBUF NOT = " "
                  AND NOT = "J"
                   AND NOT = "A"
                    AND NOT = "R"
              MOVE "STATUS MUST BE J, A, R OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO ENTER-ELE.
           MOVE VDUBUF TO LN-STATUSFG.
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDUBUF TO X-ST-BR.
           IF X-BR NOT NUMERIC
              GO TO ENTER-ELE.
           MOVE X-STATE TO ST1-KEY.
           PERFORM GET-ST1-FILE.
           IF IO-FG NOT = 0
              PERFORM INVALID-STATE
              GO TO ENTER-ELE.
           MOVE X-BR TO BR-NO.
           PERFORM GET-BR-FILE.
           IF IO-FG NOT = 0
              PERFORM INVALID-BRANCH
              GO TO ENTER-ELE.

           MOVE X-BR TO LN-OWNBR.
           MOVE X-STATE TO LN-OWNST.
           MOVE "-" TO X-HYPH.
           MOVE "S  A070130." TO VDU.
           MOVE X-ST-BR TO VDUBUF.
           PERFORM SCREENIO.

           IF ( LN-OWNBR NOT = EXT-FILPATH-BRNO )
              MOVE SPACES TO MESS
              STRING "WARNING: BRANCH NOT EQUAL USERID [",
                     EXT-CONAME-USER-LOC, "]"
                     DELIMITED BY "  " INTO MESS
              PERFORM SEND-MESS.

       A-131.
           MOVE "E  A010131." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-131.
           MOVE VDUBUF TO LN-BR-TFR-FG.
           IF LN-BR-TFR-FG NOT = "Y" AND NOT = "N"
              GO TO A-131.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDUBUF TO LN-PURCD.
           GO TO CREATE-ROUTINE.
       A-15.
           MOVE VDUBUF TO LN-COLLCD.
           GO TO CREATE-ROUTINE.
       A-16.
           MOVE VDUBUF TO LN-COLLID.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDUBUF TO LN-SRCD.
           GO TO CREATE-ROUTINE.
       A-18.
           IF VDUBUF NOT = "Y"
                  AND NOT = "N"
              PERFORM ENTER-Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-CONFIDENTCD.
           GO TO CREATE-ROUTINE.
       A-19.
           IF VDUBUF NOT = "Y"
                  AND NOT = "N"
              PERFORM ENTER-Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-ALLOTCD.
           IF LN-ALLOTCD = "Y"
              MOVE "S  A010191." TO VDU
              MOVE "N" TO VDUBUF LN-LBOX
              PERFORM SCREENIO
              GO TO A-192.
       A-191.
           MOVE "E  A010191." TO VDU
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT ENTER-KEY GO TO A-191.
           IF NOT (VDUBUF = "Y" OR "N")
              PERFORM ENTER-Y-OR-N
              GO TO A-191.
           MOVE VDUBUF TO LN-LBOX.
           IF LN-LBOX = "Y"
              MOVE "S  A010190." TO VDU
              MOVE "N" TO VDUBUF LN-ALLOTCD
              PERFORM SCREENIO.
       A-192.
           MOVE "E  A010192." TO VDU
           PERFORM SCREENIO.
           IF UP-KEY GO TO A-191.
           IF NOT ENTER-KEY GO TO A-192.
           IF NOT (VDUBUF = "Y" OR "N")
              PERFORM ENTER-Y-OR-N
              GO TO A-192.
           MOVE VDUBUF TO LN-ACH.
           GO TO CREATE-ROUTINE.
       A-20.
           IF VDUBUF NOT = "Y"
                  AND NOT = "N"
                   AND NOT = "B"
                    AND NOT = "X"
                     AND NOT = "I"
                      AND NOT = "R"
              PERFORM ENTER-Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-CONVERCD.
           GO TO CREATE-ROUTINE.
       A-21.
           IF VDUBUF NOT = "Y"
                  AND NOT = "N"
              PERFORM ENTER-Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-MIDSTREAM.
           GO TO CREATE-ROUTINE.
       A-22.
           IF VDUBUF NOT = "Y" AND
                      NOT = "N" AND
                       NOT = "E"
              MOVE "ENTER: 'Y' OR 'N' OR 'E'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-SKIP-TRW.
       A-221.
           MOVE "E  A010221." TO VDU
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT ENTER-KEY GO TO A-221.
           IF NOT (VDUBUF = "Y" OR "N")
              PERFORM ENTER-Y-OR-N
              GO TO A-221.
           MOVE VDUBUF TO LN-INTERIM-TRW.
           GO TO CREATE-ROUTINE.
       A-23.
           IF VDUBUF NOT = " "
              MOVE "ENTER A SPACE ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-RECASTCD.
           GO TO CREATE-ROUTINE.
       A-24.
           IF VDUBUF NOT = "P"
                  AND NOT = "I"
                   AND NOT = "W"
                    AND NOT = " "
              MOVE " ENTER: 'RTN' 'P' 'I' OR 'W'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-PLCD.
           IF LN-PLCD = " "
              MOVE "S  A010241." TO VDU
              MOVE " " TO LN-PLTYPE VDUBUF
              PERFORM SCREENIO
              MOVE "S  A020242." TO VDU
              MOVE " " TO LN-PLREASON VDUBUF
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
       A-241.
           MOVE "E  A010241." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-241.
           MOVE VDUBUF TO LN-PLTYPE.
           IF NOT (LN-PLTYPE = "P" OR "2" OR "3" OR "4" OR "5")
              MOVE "ENTER: 'P' OR '2' OR '3' OR '4' OR '5'" TO MESS
              PERFORM SEND-MESS
              GO TO A-241.
       A-242.
           MOVE "E  A020242." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-241.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-242.
           MOVE VDUBUF TO LN-PLREASON.
           GO TO CREATE-ROUTINE.
       A-25.
           IF VDUBUF NOT = "X"
                  AND NOT = "V"
                   AND NOT = "S"
                    AND NOT = " "
              MOVE " ENTER: 'RTN' 'X' 'V' OR 'S'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-REPOCD.
           IF NEW-FLAG NOT = 1
              IF LN-REPOCD = "X"
                      AND LN-REPODATE = 0
                 MOVE "LOAN REPO DATE = 0, PLEASE INPUT ONE!" TO MESS
                 PERFORM SEND-MESS
              ELSE
              IF LN-REPOCD = "V"
                      AND LN-REDEEMDATE = 0
                 MOVE "LOAN REPO REDEEM DATE = 0, PLEASE INPUT ONE!"
                                                                TO MESS
                 PERFORM SEND-MESS
              ELSE
              IF LN-REPOCD = "S"
                      AND LN-SOLDDATE = 0
                 MOVE "LOAN REPO SOLD DATE = 0, PLEASE INPUT ONE!"
                                                                TO MESS
                 PERFORM SEND-MESS
              ELSE
              IF LN-REPOCD = " "
                      AND (NOT (LN-REPODATE = 0 AND LN-SOLDDATE = 0))
                 MOVE "LOAN REPO DATE'S ARE SET, PLEASE CLEAR THEM!"
                                                                TO MESS
                 PERFORM SEND-MESS.
           GO TO CREATE-ROUTINE.

       A-26.
           MOVE VDUBUF TO LN-POCD TEST-POCD.

           IF ( TEST-POCD1 = "*" )
              IF ( TEST-POCD2 NOT = "P" ) AND ( NOT LN-POCD-NONLOAN )
                 MOVE "INVALID NON-LOAN PAYOFF CODE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

       A-261.
           IF NOT LN-POCD-POFF
              MOVE "S  A020261." TO VDU
              MOVE " " TO LN-PO-TRCD
              PERFORM SCREENIO
              GO TO A-262.

           MOVE "ENTER TRAN CODE USED FOR PAYOFF" TO MESS.
           PERFORM SEND-MESS.
           MOVE "E  A020261." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-261.
           MOVE VDUBUF TO LN-PO-TRCD.

       A-262.
           IF NOT LN-POCD-UNWIND
              MOVE "S  A020262." TO VDU
              MOVE " " TO LN-UNWIND-REASON-CODE
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.

           MOVE "ENTER UNWIND REASON CODE       " TO MESS.
           PERFORM SEND-MESS.
           MOVE "E  A020262." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-261.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-262.
           MOVE VDUBUF TO LN-UNWIND-REASON-CODE.
           GO TO CREATE-ROUTINE.

       A-27.
           IF VDUBUF NOT = "Y"
                  AND NOT = " "
              PERFORM ENTER-Y-OR-SP
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-ADDON-FG.
           GO TO CREATE-ROUTINE.
       A-28.
           IF VDUBUF NOT = "Y"
                  AND NOT = " "
              PERFORM ENTER-Y-OR-SP
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-PAY-SCHLD-FG.
           GO TO CREATE-ROUTINE.
       A-29.
           MOVE VDUBUF TO LN-CAN-LOAN-NO.
           GO TO CREATE-ROUTINE.
       A-30.
           MOVE VDUBUF TO LN-ACCT-FROZEN.
           IF NOT (LN-ACCT-FROZEN = "Y" OR "N")
              PERFORM ENTER-Y-OR-N
              GO TO ENTER-ELE.

       A-301.
           IF LN-ACCT-FROZEN NOT = "Y"
              MOVE "S  A020301." TO VDU
              MOVE "  " TO LN-FROZEN-REASON
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.


           MOVE "ENTER FROZEN REASON CODE" TO MESS.
           PERFORM SEND-MESS.
           MOVE "E  A020301." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-301.
           MOVE VDUBUF TO LN-FROZEN-REASON.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ************************************************
       WRITE-MODULE SECTION.
      ************************************************
           PERFORM OPEN-LN1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE LN-REC TO LNCH-REC.
           IF LNOR-REC = LNCH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF LN-REC NOT = LNOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE LNCH-REC TO LN-REC.

      *----------------------------------------------------------
      *    INSURE ALL SOC-SEC-NO'S IN LOAN RECORD ARE UNIQUE
      *----------------------------------------------------------
           PERFORM VARYING SUBX FROM 1 BY 1 UNTIL SUBX > 4
              ADD 1 TO SUBX GIVING SUBY
              PERFORM VARYING SUB FROM SUBY BY 1 UNTIL SUB > 4
                 IF (LN-SSNO(SUB) NOT = 0)
                          AND (LN-SSNO(SUB) = LN-SSNO(SUBX))
                    MOVE 0 TO LN-SSNO(SUB)
                    MOVE " " TO LN-MAKERCD(SUB)
                 END-IF
              END-PERFORM
           END-PERFORM.
      *----------------------------------------------------------
      *    INSURE NO EMPTY SOC-SEC-NO SLOTS
      *----------------------------------------------------------
           PERFORM 2 TIMES
              IF (LN-SSNO(2) = 0)
                       AND ( (LN-SSNO(3) NOT = 0)
                                    OR (LN-SSNO(4) NOT = 0)
                           )
                 MOVE LN-SSNO(3) TO LN-SSNO(2)
                 MOVE LN-MAKERCD(3) TO LN-MAKERCD(2)
                 MOVE LN-SSNO(4) TO LN-SSNO(3)
                 MOVE LN-MAKERCD(4) TO LN-MAKERCD(3)
                 MOVE 0 TO LN-SSNO(4)
                 MOVE " " TO LN-MAKERCD(4)
              END-IF
           END-PERFORM.
           IF LN-SSNO(3) = 0 AND LN-SSNO(4) NOT = 0
              MOVE LN-SSNO(4) TO LN-SSNO(3)
              MOVE LN-MAKERCD(4) TO LN-MAKERCD(3)
              MOVE 0 TO LN-SSNO(4)
              MOVE " " TO LN-MAKERCD(4).
      *----------------------------------------------------------

           PERFORM REWRITE-LN1-FILE.

           PERFORM UPDATE-NOTICE-FILE.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-LN1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED
           ELSE
              MOVE "Y" TO NEW-REC.

           PERFORM CLEAR-NO-FILE.
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.
           PERFORM WRITE-NO1-FILE.
      * WE DONT HAVE ENOUGH VALID DATA YET TO PERFORM CREATE NOTICE
      * S35Q-65
      *    PERFORM UPDATE-NOTICE-FILE.

       END-WRITE-MODULE.
           PERFORM CLOSE-LN1-FILE.

      **********************************************
       LOG-MAINTENANCE-CHGS SECTION.
      **********************************************

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LN-OWNBR  TO WORK-ERRLOG-BR.
           MOVE LN-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF LNOR-CLASS NOT = LNCH-CLASS
             MOVE "01 "                   TO WORK-ERRLOG-FIELD
             MOVE "CLASS     "            TO WORK-ERRLOG-DESC
             MOVE LNOR-CLASS              TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-CLASS              TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ORGST NOT = LNCH-ORGST
             MOVE "02 "                   TO WORK-ERRLOG-FIELD
             MOVE "ORIG STATE"            TO WORK-ERRLOG-DESC
             MOVE LNOR-ORGST              TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-ORGST              TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ORGBR NOT = LNCH-ORGBR
             MOVE "021"                   TO WORK-ERRLOG-FIELD
             MOVE "ORIG BRNO "            TO WORK-ERRLOG-DESC
             MOVE LNOR-ORGBR              TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-ORGBR              TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SPRCLASS NOT = LNCH-SPRCLASS
             MOVE "03 "                   TO WORK-ERRLOG-FIELD
             MOVE "SPR CLASS "            TO WORK-ERRLOG-DESC
             MOVE LNOR-SPRCLASS           TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-SPRCLASS           TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SUBCLASS NOT = LNCH-SUBCLASS
             MOVE "04 "                   TO WORK-ERRLOG-FIELD
             MOVE "SUB CLASS "            TO WORK-ERRLOG-DESC
             MOVE LNOR-SUBCLASS           TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-SUBCLASS           TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LAWCODE NOT = LNCH-LAWCODE
             MOVE "05 "                   TO WORK-ERRLOG-FIELD
             MOVE "LAW CODE  "            TO WORK-ERRLOG-DESC
             MOVE LNOR-LAWCODE            TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-LAWCODE            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

      *    IF LNOR-MORELN NOT = LNCH-MORELN
      *      MOVE "06 "                   TO WORK-ERRLOG-FIELD
      *      MOVE "MORE LNS"              TO WORK-ERRLOG-DESC
      *      MOVE LNOR-MORELN             TO WORK-ERRLOG-ORG-ALP
      *      MOVE LNCH-MORELN             TO WORK-ERRLOG-CHG-ALP
      *      PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SSNO(1) NOT = LNCH-SSNO(1)
             MOVE "07 "                   TO WORK-ERRLOG-FIELD
             MOVE "BORR#  1"              TO WORK-ERRLOG-DESC

             MOVE LNOR-SSNO(1)            TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-SSNO(1)            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MAKERCD(1) NOT = LNCH-MAKERCD(1)
             MOVE "07 "                   TO WORK-ERRLOG-FIELD
             MOVE "MAKERCD1"              TO WORK-ERRLOG-DESC

             MOVE LNOR-MAKERCD(1)         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-MAKERCD(1)         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SSNO(2) NOT = LNCH-SSNO(2)
             MOVE "08 "                   TO WORK-ERRLOG-FIELD
             MOVE "BORR#  2"              TO WORK-ERRLOG-DESC
             MOVE LNOR-SSNO(2)            TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-SSNO(2)            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MAKERCD(2) NOT = LNCH-MAKERCD(2)
             MOVE "08 "                   TO WORK-ERRLOG-FIELD
             MOVE "MAKERCD2"              TO WORK-ERRLOG-DESC

             MOVE LNOR-MAKERCD(2)         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-MAKERCD(2)         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SSNO(3) NOT = LNCH-SSNO(3)
             MOVE "09 "                   TO WORK-ERRLOG-FIELD
             MOVE "BORR#  3"              TO WORK-ERRLOG-DESC
             MOVE LNOR-SSNO(3)            TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-SSNO(3)            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MAKERCD(3) NOT = LNCH-MAKERCD(3)
             MOVE "09 "                   TO WORK-ERRLOG-FIELD
             MOVE "MAKERCD3"              TO WORK-ERRLOG-DESC

             MOVE LNOR-MAKERCD(3)         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-MAKERCD(3)         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SSNO(4) NOT = LNCH-SSNO(4)
             MOVE "10 "                   TO WORK-ERRLOG-FIELD
             MOVE "BORR#  4"              TO WORK-ERRLOG-DESC
             MOVE LNOR-SSNO(4)            TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-SSNO(4)            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MAKERCD(4) NOT = LNCH-MAKERCD(4)
             MOVE "10 "                   TO WORK-ERRLOG-FIELD
             MOVE "MAKERCD4"              TO WORK-ERRLOG-DESC

             MOVE LNOR-MAKERCD(4)         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-MAKERCD(4)         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-STATFLAGS NOT = LNCH-STATFLAGS
             MOVE "11 "                   TO WORK-ERRLOG-FIELD
             MOVE "STAT FLAGS"            TO WORK-ERRLOG-DESC
             MOVE LNOR-STATFLAGS          TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-STATFLAGS          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-STATUSFG NOT = LNCH-STATUSFG
             MOVE "12 "                   TO WORK-ERRLOG-FIELD
             MOVE "LAST STAT "            TO WORK-ERRLOG-DESC
             MOVE LNOR-STATUSFG           TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-STATUSFG           TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-OWNST NOT = LNCH-OWNST
             MOVE "13 "                   TO WORK-ERRLOG-FIELD
             MOVE "OWN STATE "            TO WORK-ERRLOG-DESC
             MOVE LNOR-OWNST              TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-OWNST              TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-OWNBR NOT = LNCH-OWNBR
             MOVE "131"                   TO WORK-ERRLOG-FIELD
             MOVE "OWN BRNO  "            TO WORK-ERRLOG-DESC
             MOVE LNOR-OWNBR              TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-OWNBR              TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-BR-TFR-FG NOT = LNCH-BR-TFR-FG
             MOVE "132"                   TO WORK-ERRLOG-FIELD
             MOVE "BR TFR    "            TO WORK-ERRLOG-DESC
             MOVE LNOR-BR-TFR-FG          TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-BR-TFR-FG          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PURCD NOT = LNCH-PURCD
             MOVE "14 "                   TO WORK-ERRLOG-FIELD
             MOVE "PURPOSE "              TO WORK-ERRLOG-DESC
             MOVE LNOR-PURCD              TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-PURCD              TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-COLLCD NOT = LNCH-COLLCD
             MOVE "15 "                   TO WORK-ERRLOG-FIELD
             MOVE "COLL CD "              TO WORK-ERRLOG-DESC
             MOVE LNOR-COLLCD             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-COLLCD             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-COLLID NOT = LNCH-COLLID
             MOVE "16 "                   TO WORK-ERRLOG-FIELD
             MOVE "OLD COL "              TO WORK-ERRLOG-DESC
             MOVE LNOR-COLLID             TO WORK-ERRLOG-LONG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-COLLID NOT = LNCH-COLLID
             MOVE "16 "                   TO WORK-ERRLOG-FIELD
             MOVE "NEW COL "              TO WORK-ERRLOG-DESC
             MOVE LNCH-COLLID             TO WORK-ERRLOG-LONG-ALP
             PERFORM LOG-FIELD-CHANGE
             MOVE "TO "                   TO  WORK-ERRLOG-TO-X.

           IF LNOR-SRCD NOT = LNCH-SRCD
             MOVE "17 "                   TO WORK-ERRLOG-FIELD
             MOVE "SOURCE  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-SRCD               TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-SRCD               TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-CONFIDENTCD NOT = LNCH-CONFIDENTCD
             MOVE "18 "                   TO WORK-ERRLOG-FIELD
             MOVE "CONFID  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-CONFIDENTCD        TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-CONFIDENTCD        TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ALLOTCD NOT = LNCH-ALLOTCD
             MOVE "19 "                   TO WORK-ERRLOG-FIELD
             MOVE "ALLOT CD"              TO WORK-ERRLOG-DESC
             MOVE LNOR-ALLOTCD            TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-ALLOTCD            TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LBOX NOT = LNCH-LBOX
             MOVE "191"                   TO WORK-ERRLOG-FIELD
             MOVE "LOCKBOX "              TO WORK-ERRLOG-DESC
             MOVE LNOR-LBOX              TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-LBOX              TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ACH NOT = LNCH-ACH
             MOVE "192"                   TO WORK-ERRLOG-FIELD
             MOVE "ACH FLAG"              TO WORK-ERRLOG-DESC
             MOVE LNOR-ACH                TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-ACH                TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-CONVERCD NOT = LNCH-CONVERCD
             MOVE "20 "                   TO WORK-ERRLOG-FIELD
             MOVE "CONVERTED "            TO WORK-ERRLOG-DESC
             MOVE LNOR-CONVERCD           TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-CONVERCD           TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MIDSTREAM NOT = LNCH-MIDSTREAM
             MOVE "21 "                   TO WORK-ERRLOG-FIELD
             MOVE "MIDSTREAM"             TO WORK-ERRLOG-DESC
             MOVE LNOR-MIDSTREAM          TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-MIDSTREAM          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SKIP-TRW NOT = LNCH-SKIP-TRW
             MOVE "22 "                   TO WORK-ERRLOG-FIELD
             MOVE "SKIP TRW"              TO WORK-ERRLOG-DESC
             MOVE LNOR-SKIP-TRW           TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-SKIP-TRW           TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INTERIM-TRW NOT = LNCH-INTERIM-TRW
             MOVE "221"                   TO WORK-ERRLOG-FIELD
             MOVE "INTER TRW "            TO WORK-ERRLOG-DESC
             MOVE LNOR-INTERIM-TRW        TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INTERIM-TRW        TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-RECASTCD NOT = LNCH-RECASTCD
             MOVE "23 "                   TO WORK-ERRLOG-FIELD
             MOVE "RECASTCD"              TO WORK-ERRLOG-DESC
             MOVE LNOR-RECASTCD           TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-RECASTCD           TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PLCD NOT = LNCH-PLCD
             MOVE "24 "                   TO WORK-ERRLOG-FIELD
             MOVE "P&L CODE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-PLCD               TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-PLCD               TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PLTYPE NOT = LNCH-PLTYPE
             MOVE "241"                   TO WORK-ERRLOG-FIELD
             MOVE "P&L TYPE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-PLTYPE             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-PLTYPE             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PLREASON NOT = LNCH-PLREASON
             MOVE "242"                   TO WORK-ERRLOG-FIELD
             MOVE "P&L REASON"            TO WORK-ERRLOG-DESC
             MOVE LNOR-PLREASON           TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-PLREASON           TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-REPOCD NOT = LNCH-REPOCD
             MOVE "25 "                   TO WORK-ERRLOG-FIELD
             MOVE "REPO CD "              TO WORK-ERRLOG-DESC
             MOVE LNOR-REPOCD             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-REPOCD             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-POCD NOT = LNCH-POCD
             MOVE "26 "                   TO WORK-ERRLOG-FIELD
             MOVE "POFF CODE "            TO WORK-ERRLOG-DESC
             MOVE LNOR-POCD               TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-POCD               TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PO-TRCD NOT = LNCH-PO-TRCD
             MOVE "261"                   TO WORK-ERRLOG-FIELD
             MOVE "POFF TRCD "            TO WORK-ERRLOG-DESC
             MOVE LNOR-PO-TRCD            TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-PO-TRCD            TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-UNWIND-REASON-CODE NOT = LNCH-UNWIND-REASON-CODE
             MOVE "262"                   TO WORK-ERRLOG-FIELD
             MOVE "UNWIND  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-UNWIND-REASON-CODE TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-UNWIND-REASON-CODE TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ADDON-FG NOT = LNCH-ADDON-FG
             MOVE "27 "                   TO WORK-ERRLOG-FIELD
             MOVE "ADD-ON  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-ADDON-FG           TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-ADDON-FG           TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PAY-SCHLD-FG NOT = LNCH-PAY-SCHLD-FG
             MOVE "28 "                   TO WORK-ERRLOG-FIELD
             MOVE "PAY SCH "              TO WORK-ERRLOG-DESC
             MOVE LNOR-PAY-SCHLD-FG       TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-PAY-SCHLD-FG       TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-CAN-LOAN-NO NOT = LNCH-CAN-LOAN-NO
             MOVE "29 "                   TO WORK-ERRLOG-FIELD
             MOVE "CAN LOAN"              TO WORK-ERRLOG-DESC
             MOVE LNOR-CAN-LOAN-NO        TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-CAN-LOAN-NO        TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ACCT-FROZEN NOT = LNCH-ACCT-FROZEN
             MOVE "30 "                   TO WORK-ERRLOG-FIELD
             MOVE "FROZEN  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-ACCT-FROZEN        TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-ACCT-FROZEN        TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-FROZEN-REASON NOT = LNCH-FROZEN-REASON
             MOVE "30 "                   TO WORK-ERRLOG-FIELD
             MOVE "FROZ REAS"              TO WORK-ERRLOG-DESC
             MOVE LNOR-ACCT-FROZEN        TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-ACCT-FROZEN        TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

      ************************************
       LOG-FIELD-CHANGE SECTION.
      ************************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      ***********************************
       DELETE-MODULE SECTION.
      ***********************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF LN-REC NOT = LNOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-LN1-FILE.

      * WRITE CHANGES FILE
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY
           STRING LN1-KEY, " ", "DELETED" DELIMITED BY SIZE
                INTO ERRLOGW-KEY.

           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

           PERFORM DELETE-NOTICE-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-LN1-FILE.

      ***********************************************
       UPDATE-NOTICE-FILE SECTION.
      ***********************************************
           PERFORM OPEN-NO1-FILE.
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO CRNOR2-ACCTNO.
           PERFORM READ-NO1-FILE.

           ACCEPT CRNOR2-SYSDATE FROM CENTURY-DATE.
           MOVE "CL/CRNOR2"    TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.
           IF IO-FG = 0
              PERFORM REWRITE-NO1-FILE
           ELSE
              PERFORM WRITE-NO1-FILE.

           PERFORM CLOSE-NO1-FILE.
       EXIT-UPDATE-NOTICE-FILE.
           EXIT.

      ***********************************
       NEW-RECORD-SETUP SECTION.
      ***********************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-LN-FILE.
           MOVE 0 TO ELE.

       NEW-RECORD-EXIT.
           EXIT.

      **********************************
       DISPLAY-FIELDS SECTION.
      **********************************
           MOVE LN-CLASS TO F01.
           MOVE "-" TO X-HYPH.
           MOVE LN-ORGBR TO X-BR.
           MOVE LN-ORGST TO X-STATE.
           MOVE X-ST-BR TO F02.
           MOVE LN-SPRCLASS TO F03.
           MOVE LN-SUBCLASS TO F04.
           MOVE LN-LAWCODE TO F05.
      *    MOVE LN-MORELN TO F06.
           MOVE LN-STATFLAGS TO F11.
           MOVE LN-STATUSFG TO F12.
           MOVE LN-OWNBR TO X-BR.
           MOVE LN-OWNST TO X-STATE.
           MOVE X-ST-BR TO F13.
           MOVE LN-BR-TFR-FG TO F131.
           MOVE LN-PURCD TO F14.
           MOVE LN-COLLCD TO F15.
           MOVE LN-COLLID TO F16.
           MOVE LN-SRCD TO F17.
           MOVE LN-CONFIDENTCD TO F18.
           MOVE LN-ALLOTCD TO F19.
           MOVE LN-LBOX TO F191.
           MOVE LN-ACH TO F192.
           MOVE LN-CONVERCD TO F20.
           MOVE LN-MIDSTREAM TO F21.
           MOVE LN-SKIP-TRW TO F22.
           MOVE LN-INTERIM-TRW TO F221.
           MOVE LN-RECASTCD TO F23.
           MOVE LN-PLCD TO F24.
           MOVE LN-PLTYPE TO F241.
           MOVE LN-PLREASON TO F242.
           MOVE LN-REPOCD TO F25.
           MOVE LN-POCD               TO F26.
           MOVE LN-PO-TRCD            TO F261.
           MOVE LN-UNWIND-REASON-CODE TO F262.
           MOVE LN-ADDON-FG TO F27.
           MOVE LN-PAY-SCHLD-FG TO F28.
           MOVE LN-CAN-LOAN-NO TO F29.
           MOVE LN-ACCT-FROZEN TO F30.
           MOVE LN-FROZEN-REASON TO TEST-FROZEN-REASON.
           IF TEST-FROZEN-REASON1 = X"00"
              MOVE SPACES TO F301
           ELSE
              MOVE LN-FROZEN-REASON TO F301.

           MOVE LN-SMALLOAN  TO FSMALLOAN.
           MOVE LN-SALEFIN   TO FSALEFIN.
           MOVE LN-SERVACCT  TO FSERVACCT.
           MOVE LN-LOANTYPE  TO FLOANTYPE.
           MOVE LN-GIFTNET   TO FGIFTNET.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM DISPLAY-THE-BORROWERS
               VARYING INDX FROM 1 BY 1 UNTIL INDX > 4.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      *********************************************
       DISPLAY-THE-BORROWERS SECTION.
      *********************************************
           COMPUTE VDU-INDX = INDX + 6.
           MOVE LN-SSNO(INDX) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           MOVE "S  X110 " TO VDU.
           MOVE VDU-INDX TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           IF LN-SSNO(INDX) NOT = 0
              MOVE LN-SSNO(INDX) TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.
           MOVE "S  A200 " TO VDU.
           MOVE VDU-INDX TO VDUROW.
           MOVE 2 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                                        INTO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010 " TO VDU.
           MOVE VDU-INDX TO VDUROW.
           MOVE 1 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           MOVE SPACES TO VDUBUF.
           MOVE LN-MAKERCD(INDX) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE SPACES TO BW-REC.

      *********************************************
       VERIFY-DELETE SECTION.
      *********************************************
           MOVE 0 TO IO-FG.
           IF LN-CURBAL NOT = LN-LNAMT
              MOVE "LOAN CURRENT BALANCE NOT = LOAN AMT" TO MESS
              PERFORM ERROR-MESSAGE.
           IF LN-INTBAL NOT = 0
              MOVE "LOAN INTEREST BALANCE NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.
           IF LN-OTHBAL NOT = 0
              MOVE "LOAN OTHER BALANCE NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.
           IF LN-LCBAL NOT = 0
              MOVE "LATE CHARGE BALANCE NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.
           IF LN-POFFDATE NOT = 0
              MOVE "LOAN PAYOFF DATE NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.
           IF NOT ( LN-POCD-OPEN )
              MOVE "LOAN PAYOFF CODE NOT = SPACES" TO MESS
              PERFORM ERROR-MESSAGE.
           GO TO EXIT-VERIFY-DELETE.
       ERROR-MESSAGE.
           PERFORM SEND-MESS.
           MOVE 9 TO IO-FG.
       EXIT-VERIFY-DELETE.
           IF IO-FG = 9
              MOVE "ACTIVE LOAN, DELETE IGNORED !" TO MESS
              PERFORM SEND-MESS.

      ******************************************
       TEST-RESCISSION-STATUS SECTION.
      ******************************************
           IF ( LN-POCD-INRESCISSION )
              MOVE "ACCOUNT IS CURRENTLY IN RESCISSION" TO MESS
              PERFORM SEND-MESS.
           IF ( LN-POCD-PB-RESC ) AND ( PAGE-CHANGE-NO )
              MOVE "ACCOUNT IS IN RESCISSION RENEWAL" TO MESS
              PERFORM SEND-MESS.

      ***********************************************************
       FORM-RESET SECTION.
      ***********************************************************
           MOVE EXT-CONAME-CONAME TO LNMAN1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LNMAN1-SYSDATE.
           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LNMAN1-SCREEN UPON LNMAIN-WINDOW-HANDLE.
           MOVE LNMAN1-FORM-FIELDS TO WR-BUF.
           MOVE LNMAN1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      ***********************************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************************
           MOVE LNMAN1-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNMAN1-HELP--DIR TO HELP-LINK-DIR.

      ***********************************************************
       VDU-CONVERT-FIELD SECTION.
      ***********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/LNMAN1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************************
       MISC-PARAGRAPHS SECTION.
      ***************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       DELETE-NOTICE-FILE.
           PERFORM OPEN-NO1-FILE.
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO CRNOR2-ACCTNO.
           PERFORM READ-NO1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-NO1-FILE.
           PERFORM CLOSE-NO1-FILE.
       ENTER-Y-OR-N.
           MOVE "ENTER: 'Y' OR 'N'" TO MESS.
           PERFORM SEND-MESS.
       ENTER-Y-OR-SP.
           MOVE "ENTER: 'Y' OR ' '" TO MESS.
           PERFORM SEND-MESS.
       GET-BR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
       GET-ST1-FILE.
           PERFORM OPEN-ST1-FILE.
           PERFORM READ-ST1-FILE.
           PERFORM CLOSE-ST1-FILE.
       TEST-SPR.
           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
       INVALID-BRANCH.
           MOVE "INVALID BRANCH NO" TO MESS.
           PERFORM SEND-MESS.
       INVALID-STATE.
           MOVE "INVALID STATE CODE" TO MESS.
           PERFORM SEND-MESS.
       INVALID-SPR.
           MOVE "STATE PARAMETER RECORD NOT ON FILE" TO MESS.
           PERFORM SEND-MESS.
       VALIDATE-LN-CLASS.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE " INVALID LOAN CLASS" TO MESS
              PERFORM SEND-MESS.
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE "ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.
       NOT-EXPIRED.
           MOVE "THIS RECORD HAS NOT EXPIRED - CANNOT DELETE" TO MESS.
           PERFORM SEND-MESS.

      ***********************************
       MODIFY-BORROWER SECTION.
      ***********************************
           MOVE "N" TO BW-SKIP.
           IF VDUNUM0 = 0
              MOVE "Y" TO BW-SKIP
              MOVE 0 TO LN-SSNO(INDX)
              MOVE SPACES TO BW-FNAME BW-LNAME LN-MAKERCD(INDX)
           ELSE
              PERFORM MODIFY-SSN.

           MOVE "S  X110 " TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           MOVE LN-SSNO(INDX) TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A200 " TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 2 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                                        INTO VDUBUF.
           PERFORM SCREENIO.

           IF BW-SKIP = "Y"
              GO TO MB-CD.

      * BORROWER SSNO NOT FOUND IN BWFILE
           IF BW-SKIP = "X"
              GO TO MB-EXIT.
       MB-MC.
           MOVE "E  A010 " TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 1 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
           MOVE VDUBUF TO WK-MAKERCD.
           IF NOT GOOD-MAKERCD
              MOVE "ENTER: 'M' 'J' OR 'E'" TO MESS
              PERFORM SEND-MESS
              GO TO MB-MC.
           MOVE VDUBUF TO LN-MAKERCD(INDX).
       MB-CD.
           MOVE "S  A010 " TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 1 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           MOVE SPACES TO VDUBUF.
           MOVE LN-MAKERCD(INDX) TO VDUBUF.
           PERFORM SCREENIO.
       MB-EXIT.
           EXIT.

      ****************************************
       MODIFY-SSN SECTION.
      ****************************************
           MOVE VDUNUM0 TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE "X" TO BW-SKIP
              MOVE "* NOT ON FILE *" TO MESS
              PERFORM SEND-MESS
           ELSE
              MOVE BW-SSNO TO LN-SSNO(INDX).

       COPY "LIBLP/LPNOCL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPLNCL.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ***************************************
       IO-ROUTINES SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ***************************************
       END-ROUTINE SECTION.
      ***************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
           DESTROY LNMAN1-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE LNMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
