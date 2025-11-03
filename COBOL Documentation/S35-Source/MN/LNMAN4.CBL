       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNMAN4.
      ******************************************************************
      *
      *   DIR :  MN
      *   DESC: LOAN MASTER FILE MAINTENANCE   PAGE 4
      *
      *         INSURANCE INFORMATION
      *
      *=================================================================
      * REV:
      * MJD 050895 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      * JTG 070995 ADDED LN-INSPREM-DLADJ, ADJUSTMENT WHEN DEALER
      *                  WROTE OUR INS. PREMIUMS WRONG
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * JTG 081297 ADDED LN-FEEFINCHG & LN-INSFINCHG FLAG INPUT
      * MJD 082197 CHANGED RECORD SIZE FROM 1500 TO 1570
      *            ADD LOGIC FOR PAGE 10.
      * BAH 110397 CHANGED TO USE TIM360(LIKE A48) FOR "M" MONTHLY ACCOUNTS,
      *            IN THE INSURANCE TERM CALCULATION ROUTINE,
      *            BECAUSE 10/31/97 TERM 18, 1STPYDATE 11/30/97, MDTE
      *            4/30/99 WAS CAUSING INSURANCE TERM OF 17....
      * MJD 112197 EXPANDED LN-REC TO 1800 BYTES
      * KEC 090398 DISPLAYING LN-REVOLVING FOR 'LNSTAT'.
      * KEC 091598 ADDED LOGIC TO ACCESS NEW PAGE 11.
      * JTG 120298 ADDED LOGIC TO ACCESS NEW PAGE 12.
      * BLV 020610 ADDED FIELD # 24 FOR WORLD, LN-AD-D-INS-COVR
      *
      * KEC 020624 {PR#01823} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *           ADDED LOGIC FOR USING 'CD-GC-FEE-MAXAMT'
      *
      * BAH 040108 ADDED F3 FROM THE FEECODE FIELDS TO CALL FEE PAYABLE
      *            MAINTENANCE (MN/FPMAIN), WORLD 331
      * AMM 051018 ADDED DETAILED, FIELD-BY-FIELD MAINTENANCE LOG IN
      *            /USR/DATA/R1/LG/CHG.YYMMDD
      * BAH 100224 CHANGED BRINSEARN FROM CONAME TO READ BR-FILE AND USE
      *             BR-AUTO-INS-EARN, PL# 676
      * MJD 121025 A30 CONVERSION COMPLETE.
      * MJD 121217 ADDED LNMAIN-WINDOW-HANDLE TO SCREEN DISPLAY
      * MJD 130117 CHANGED CALL TMMSG TO PERFORM CALL-TMMSG
      *
      * KEC 2016.0316 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      * CS 160616 REMOVE LN-SEQNO
      * CS 160711 SETUP LN-OWNBR WILL BECOME PART OF KEY
      * CS 170425 ACTIVATE 8 DIGIT DATES
      * KEC 2017.0526 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED ORIGINAL-REC AND CHANGED-REC WHICH NOW USING
      *                  LNOR-REC AND LNCH-REC.
      * BAH 181212 GLOBAL LNFILE
      * BAH 20190627 REMOVED LN-REVOLVING, PT0153
      * BAH 2024.0131 CALL CRNOR2 AND WRITE NOTICE RECORD
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/LNMAN4.CBL S35 02/21/24-12:33:37 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01IC.CPY".
       COPY "LIBLP/LP01IC_SQL.CPY".
       COPY "LIBLP/LPWSIC.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

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
           03  WORK-ERRLOG-DESC         PIC X(8).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(13).
               05  WORK-ERRLOG-ORG-N0 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(12)-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(9).99-.

               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(13).
               05  WORK-ERRLOG-CHG-N0 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(12)-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(9).99-.

       01  ONE               PIC S9.
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
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-TIME        PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.
       01  WK-INSURED        PIC X VALUE " ".
           88  GOOD-INSURED  VALUE "N" "M" "S" "J".
       01  I-LEVEL           PIC 99 VALUE 0.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  PAGE-CHANGE-FLAG      PIC X(1)    VALUE "N".
           88  PAGE-CHANGE-NO                VALUE "N".
           88  PAGE-CHANGE-YES               VALUE "Y".

       01 LNMAIN-LINK.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(8).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  LNMAIN-WINDOW-HANDLE  PIC X(10).

       01  WS-ORGBR          PIC 9999.

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  F01        PIC X.
           03  F02        PIC X.
           03  F021       PIC X(18).
           03  F03        PIC X.
           03  F031       PIC X(18).
           03  F04        PIC ZZ.
           03  F041       PIC X.
           03  F042       PIC ZZ/ZZ/ZZ.
           03  F043       PIC ZZ/ZZ/ZZ.
           03  F044       PIC ZZZ.ZZ.
           03  F045       PIC Z(6).ZZ.
           03  F046       PIC Z(4).ZZ.
           03  F047       PIC Z(4).ZZ.
           03  F048       PIC X.

           03  F05        PIC ZZ.
           03  F051       PIC X.
           03  F052       PIC ZZ/ZZ/ZZ.
           03  F053       PIC ZZ/ZZ/ZZ.
           03  F054       PIC ZZZ.ZZ.
           03  F055       PIC Z(6).ZZ.
           03  F056       PIC Z(4).ZZ.
           03  F057       PIC Z(4).ZZ.
           03  F058       PIC X.

           03  F06        PIC ZZ.
           03  F061       PIC X.
           03  F062       PIC ZZ/ZZ/ZZ.
           03  F063       PIC ZZ/ZZ/ZZ.
           03  F064       PIC ZZZ.ZZ.
           03  F065       PIC Z(6).ZZ.
           03  F066       PIC Z(4).ZZ.
           03  F067       PIC Z(4).ZZ.
           03  F068       PIC X.

           03  F07        PIC ZZ.
           03  F071       PIC X.
           03  F072       PIC ZZ/ZZ/ZZ.
           03  F073       PIC ZZ/ZZ/ZZ.
           03  F074       PIC ZZZ.ZZ.
           03  F075       PIC Z(6).ZZ.
           03  F076       PIC Z(4).ZZ.
           03  F077       PIC Z(4).ZZ.
           03  F078       PIC X.

           03  F08        PIC ZZ.
           03  F081       PIC X.
           03  F082       PIC ZZ/ZZ/ZZ.
           03  F083       PIC ZZ/ZZ/ZZ.
           03  F084       PIC ZZZ.ZZ.
           03  F085       PIC Z(6).ZZ.
           03  F086       PIC Z(4).ZZ.
           03  F087       PIC Z(4).ZZ.
           03  F088       PIC X.

           03  F09        PIC ZZ.
           03  F091       PIC X.
           03  F092       PIC ZZ/ZZ/ZZ.
           03  F093       PIC ZZ/ZZ/ZZ.
           03  F094       PIC ZZZ.ZZ.
           03  F095       PIC Z(6).ZZ.
           03  F096       PIC Z(4).ZZ.
           03  F097       PIC Z(4).ZZ.
           03  F098       PIC X.

           03  F10        PIC ZZ.
           03  F101       PIC X.
           03  F102       PIC ZZ/ZZ/ZZ.
           03  F103       PIC ZZ/ZZ/ZZ.
           03  F104       PIC ZZZ.ZZ.
           03  F105       PIC Z(6).ZZ.
           03  F106       PIC Z(4).ZZ.
           03  F107       PIC Z(4).ZZ.
           03  F108       PIC X.

           03  F11        PIC ZZ.
           03  F111       PIC X.
           03  F112       PIC ZZ/ZZ/ZZ.
           03  F113       PIC ZZ/ZZ/ZZ.
           03  F114       PIC ZZZ.ZZ.
           03  F115       PIC Z(6).ZZ.
           03  F116       PIC Z(4).ZZ.
           03  F117       PIC Z(4).ZZ.
           03  F118       PIC X.


      * 12. INS CONFIRMATION DATE
           03  F12        PIC ZZ/ZZ/ZZ.
      * 13. INSPREM DLADJ
           03  F13        PIC ZZZZ9.99-.
      * 131 DL O3 COMM
           03  F131       PIC ZZZZ9.99-.

           03  FFEES          OCCURS 10.
               05  FFEECD     PIC XX.
               05  FFEEDESC   PIC X(17).
               05  FFEEAMT    PIC Z(4).ZZ.
               05  FFEEOURS   PIC X.
               05  FFEEFINCHG PIC X.

           03  F24        PIC ZZZZZ9.
           03  FLNSTAT    PIC X(30).

       01  DISPLAY-LIST.
           03  FILLER     PIC X(20) VALUE
               "010 020 021 030 031 ".
           03  FILLER     PIC X(36) VALUE
               "040 041 042 043 044 045 046 047 048 ".
           03  FILLER     PIC X(36) VALUE
               "050 051 052 053 054 055 056 057 058 ".
           03  FILLER     PIC X(36) VALUE
               "060 061 062 063 064 065 066 067 068 ".
           03  FILLER     PIC X(36) VALUE
               "070 071 072 073 074 075 076 077 078 ".
           03  FILLER     PIC X(36) VALUE
               "080 081 082 083 084 085 086 087 088 ".
           03  FILLER     PIC X(36) VALUE
               "090 091 092 093 094 095 096 097 098 ".
           03  FILLER     PIC X(36) VALUE
               "100 101 102 103 104 105 106 107 108 ".
           03  FILLER     PIC X(36) VALUE
               "110 111 112 113 114 115 116 117 118 ".
           03  FILLER     PIC X(32) VALUE
               "120 130 131 140 141 142 143 144 ".
           03  FILLER     PIC X(40) VALUE
               "150 151 152 153 154 160 161 162 163 164 ".
           03  FILLER     PIC X(40) VALUE
               "170 171 172 173 174 180 181 182 183 184 ".
           03  FILLER     PIC X(40) VALUE
               "190 191 192 193 194 200 201 202 203 204 ".
           03  FILLER     PIC X(40) VALUE
               "210 211 212 213 214 220 221 222 223 224 ".
           03  FILLER     PIC X(32) VALUE
               "230 231 232 233 234 240 LNSTAT.".
      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       01  MAX            PIC 99   VALUE 24.
       01  SPEC-TABLE.
           03  FILLER     PIC X(8) VALUE "E  A010N".
           03  FILLER     PIC X(8) VALUE "E  A010N".
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 4. CL INSURANCE
           03  FILLER     PIC X(8) VALUE "ENNN020N".
           03  FILLER     PIC X(8) VALUE "ENNN020N".
           03  FILLER     PIC X(8) VALUE "ENNN020N".
           03  FILLER     PIC X(8) VALUE "ENNN020N".
           03  FILLER     PIC X(8) VALUE "ENNN020N".
           03  FILLER     PIC X(8) VALUE "ENNN020N".
           03  FILLER     PIC X(8) VALUE "ENNN020N".
      * 11. O5 INSURANCE
           03  FILLER     PIC X(8) VALUE "ENNN020N".
      * 12. LN-INS-CONFIRM-DATE
           03  FILLER     PIC X(8) VALUE "ERNM060N".
      * 13. INSURANCE PREMIUM DEALER ADJ
           03  FILLER     PIC X(8) VALUE "ENNS052N".
      * 14. FEE 1
           03  FILLER     PIC X(8) VALUE "E  A020N".
           03  FILLER     PIC X(8) VALUE "E  A020N".
           03  FILLER     PIC X(8) VALUE "E  A020N".
           03  FILLER     PIC X(8) VALUE "E  A020N".
           03  FILLER     PIC X(8) VALUE "E  A020N".
           03  FILLER     PIC X(8) VALUE "E  A020N".
           03  FILLER     PIC X(8) VALUE "E  A020N".
           03  FILLER     PIC X(8) VALUE "E  A020N".
           03  FILLER     PIC X(8) VALUE "E  A020N".
           03  FILLER     PIC X(8) VALUE "E  A020N".
      * 24. AD&D INS COVERAGE AMT
           03  FILLER     PIC X(8) VALUE "ENNN060N".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 24 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/LNMAN4_DEF.CPY".
       COPY "LIBMN/LNMAN4_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETFMW.CPY".

       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/LNMAN4_SCN.CPY".
       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ***********************************************
       MAIN-PROGRAM SECTION.
      ***********************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LNMAN4" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO LNMAIN-LINK.

       MAIN-MODULE.
           MOVE " " TO VDU-PGMENU-SELECTION.
           PERFORM FORM-RESET.
           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME.

           PERFORM REC-LN-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "N" TO BR-AUTO-INS-EARN.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P1
              MOVE 5 TO FORM-SEL1.

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

           GO TO MAIN-MODULE.

      ****************************
       REC-LN-MODULE SECTION.
      ****************************
       REC-LN-MODULE-NO.
           MOVE 0 TO LN-ACCTNO LN-OWNBR FIRST-SCAN.
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE "Y" TO PAGE-CHANGE-FLAG
              MOVE SAVEKEY TO LN-ACCTNO
              MOVE SPACES TO SAVEKEY
              PERFORM DISPLAY-SCAN-KEY
              GO TO READ-LNFILE.

           MOVE "N" TO PAGE-CHANGE-FLAG.
           MOVE 0 TO NEW-FLAG.

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
       READ-LNFILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LN-MODULE-NO.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LN-MODULE-NO
           ELSE
           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LN-MODULE-NO
           ELSE
              MOVE LN-REC TO LNOR-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
              IF NEW-REC = "Y"
                 MOVE 1 TO NEW-FLAG
              END-IF
              IF ( LN-POCD-INRESCISSION ) AND ( PAGE-CHANGE-NO )
                 MOVE "ACCOUNT IS CURRENTLY IN RESCISSION" TO MESS
                 PERFORM SEND-MESS
              END-IF.

       END-REC-LN-MODULE.

      ***********************************************
       ENTER-SCANKEY SECTION.
      ***********************************************
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
                 MOVE LN-REC TO LNOR-REC
                 IF ( LN-POCD-INRESCISSION )
                    MOVE "ACCOUNT IS CURRENTLY IN RESCISSION" TO MESS
                    PERFORM SEND-MESS
                 END-IF
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.
           MOVE VDUNUM0       TO LN-ACCTNO.
           MOVE LN-PATH-OWNBR TO LN-OWNBR.


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
      *  ENTRY-MODULE
      *  NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *             1 - INITIAL NEW RECORD ENTRY
      *             2 - MODIFY NEW RECORD
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
              GO TO END-ENTRY-MODULE.
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 1 OR FORM-SEL1 = 4 OR FORM-SEL1 > 9
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
      * FORM-NUM2 CHECKS FOR PAGES 10 THRU 99
           IF FORM-KEY2 = " " AND FORM-P2 = "P" AND FORM-SEL2 NUMERIC
              IF NOT FORM-2-DIGITS
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
           GO TO CHECK-CHNO.
       CHECK-PAGE.
           MOVE "RWR" TO IO-TYPE.
           GO TO EXIT-ENTRY-MODULE.
       CHECK-CHNO.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
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
           IF NEW-FLAG = 0
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
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F3.
           IF ELE = 14 OR 15 OR 16 OR 17 OR 18 OR 19 OR 20 OR 21
                        OR 22 OR 23
              IF VDUSTATUS = "16"
                 MOVE "MN/FPMAIN" TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                 CANCEL FORM-PROGX.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO A-01 A-02 A-03 A-04 A-05
                 A-06 A-07 A-08 A-09 A-10
                 A-11 A-12 A-13 A-14 A-15
                 A-16 A-17 A-18 A-19 A-20
                 A-21 A-22 A-23 A-24
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           MOVE VDUBUF TO WK-INSURED.
           IF NOT GOOD-INSURED
              MOVE "ENTER: 'N' 'M' 'S' 'J'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-INSURED.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUBUF TO LN-INSTYPES.
           IF NOT LN-INSTYPES-VALID
              GO TO ENTER-ELE.
           MOVE "S  A170 " TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 1 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           IF LN-INSTYPES = "A" MOVE "LL, O1" TO VDUBUF
           ELSE IF LN-INSTYPES = "B" MOVE "LL" TO VDUBUF
           ELSE IF LN-INSTYPES = "C" MOVE "DL" TO VDUBUF
           ELSE IF LN-INSTYPES = "D" MOVE "A&H" TO VDUBUF
           ELSE IF LN-INSTYPES = "E" MOVE "PROP" TO VDUBUF
           ELSE IF LN-INSTYPES = "F" MOVE "LL, AH" TO VDUBUF
           ELSE IF LN-INSTYPES = "G" MOVE "DL, AH" TO VDUBUF
           ELSE IF LN-INSTYPES = "H" MOVE "LL, PP" TO VDUBUF
           ELSE IF LN-INSTYPES = "I" MOVE "DL, O1" TO VDUBUF
           ELSE IF LN-INSTYPES = "J" MOVE "DL, PP" TO VDUBUF
           ELSE IF LN-INSTYPES = "K" MOVE "AH, PP" TO VDUBUF
           ELSE IF LN-INSTYPES = "L" MOVE "LL, AH, PP" TO VDUBUF
           ELSE IF LN-INSTYPES = "M" MOVE "DL, AH, PP" TO VDUBUF
           ELSE IF LN-INSTYPES = "N" MOVE "NONE" TO VDUBUF
           ELSE IF LN-INSTYPES = "O" MOVE "LL, AH, O1" TO VDUBUF
           ELSE IF LN-INSTYPES = "P" MOVE "DL, AH, O1" TO VDUBUF
           ELSE IF LN-INSTYPES = "Q" MOVE "AH, O1" TO VDUBUF
           ELSE IF LN-INSTYPES = "R" MOVE "PROP2" TO VDUBUF
           ELSE IF LN-INSTYPES = "S" MOVE "LL, PP2" TO VDUBUF
           ELSE IF LN-INSTYPES = "T" MOVE "DL, PP2" TO VDUBUF
           ELSE IF LN-INSTYPES = "U" MOVE "LL, PP, O1" TO VDUBUF
           ELSE IF LN-INSTYPES = "V" MOVE "AH, PP2" TO VDUBUF
           ELSE IF LN-INSTYPES = "W" MOVE "LL, AH, PP2" TO VDUBUF
           ELSE IF LN-INSTYPES = "X" MOVE "DL, AH, PP2" TO VDUBUF
           ELSE IF LN-INSTYPES = "Y" MOVE "DL, PP, O1" TO VDUBUF
           ELSE IF LN-INSTYPES = "Z" MOVE "PP, O1" TO VDUBUF
           ELSE IF LN-INSTYPES = "1" MOVE "O1" TO VDUBUF
           ELSE IF LN-INSTYPES = "2" MOVE "LL, PP2, O1" TO VDUBUF
           ELSE IF LN-INSTYPES = "3" MOVE "LL, AH, PP, O1" TO VDUBUF
           ELSE IF LN-INSTYPES = "4" MOVE "LL, AH, PP2, O1" TO VDUBUF
           ELSE IF LN-INSTYPES = "5" MOVE "DL, PP2, O1" TO VDUBUF
           ELSE IF LN-INSTYPES = "6" MOVE "DL, AH, PP, O1" TO VDUBUF
           ELSE IF LN-INSTYPES = "7" MOVE "DL, AH, PP2, O1" TO VDUBUF
           ELSE IF LN-INSTYPES = "8" MOVE "PP2 ,O1" TO VDUBUF
           ELSE IF LN-INSTYPES = "9" MOVE "AH, PP, O1" TO VDUBUF
           ELSE IF LN-INSTYPES = "0" MOVE "AH, PP2, O1" TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUBUF TO LN-INSELIGIBLE.
           IF NOT LN-INSELIGIBLE-VALID
              GO TO ENTER-ELE.
           MOVE "S  A170 " TO VDU
           MOVE ELE TO VDUROW.
           MOVE 1 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           IF LN-INSELIGIBLE = "A" MOVE "LL, O1" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "B" MOVE "LL" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "C" MOVE "DL" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "D" MOVE "A&H" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "E" MOVE "PROP" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "F" MOVE "LL, AH" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "G" MOVE "DL, AH" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "H" MOVE "LL, PP" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "I" MOVE "DL, O1" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "J" MOVE "DL, PP" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "K" MOVE "AH, PP" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "L" MOVE "LL, AH, PP" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "M" MOVE "DL, AH, PP" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "N" MOVE "NONE" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "O" MOVE "LL, AH, O1" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "P" MOVE "DL, AH, O1" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "Q" MOVE "AH, O1" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "R" MOVE "PROP2" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "S" MOVE "LL, PP2" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "T" MOVE "DL, PP2" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "U" MOVE "LL, PP, O1" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "V" MOVE "AH, PP2" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "W" MOVE "LL, AH, PP2" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "X" MOVE "DL, AH, PP2" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "Y" MOVE "DL, PP, O1" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "Z" MOVE "PP, O1" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "1" MOVE "O1" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "2" MOVE "LL, PP2, O1" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "3" MOVE "LL, AH, PP, O1" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "4" MOVE "LL, AH, PP2, O1" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "5" MOVE "DL, PP2, O1" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "6" MOVE "DL, AH, PP, O1" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "7" MOVE "DL, AH, PP2, O1" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "8" MOVE "PP2 ,O1" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "9" MOVE "AH, PP, O1" TO VDUBUF
           ELSE IF LN-INSELIGIBLE = "0" MOVE "AH, PP2, O1" TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE 1 TO I-LEVEL.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE 2 TO I-LEVEL.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE 3 TO I-LEVEL.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE 4 TO I-LEVEL.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE 5 TO I-LEVEL.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE 6 TO I-LEVEL.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE 7 TO I-LEVEL.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE 8 TO I-LEVEL.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDU-DATE-YYYYMMDD TO LN-INS-CONFIRM-DATE.
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDUNUM2 TO LN-INSPREM-DLADJ.
           IF LN-INSPREM-DLADJ NOT = 0
              IF LN-SALEFIN = "N"
                 MOVE "THIS FIELD MUST BE ZERO ON DIRECT LOANS"
                                                   TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
       A-131.
           MOVE "ENNS052131." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-131.
           MOVE VDUNUM2  TO LN-O3INS-DLPROC.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE 1 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-15.
           MOVE 2 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-16.
           MOVE 3 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE 4 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE 5 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE 6 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE 7 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-21.
           MOVE 8 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE 9 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE 10 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-24.
           MOVE VDUNUM0 TO LN-AD-D-INS-COVR.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ***********************************************
       INSURANCE-ENTRY SECTION.
      ***********************************************
           MOVE 0 TO IO-FG.
           IF VDUNUM0 = 0
              MOVE "SN N020  0." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO LN-INSCOMP(I-LEVEL) VDUNUM0
              PERFORM SCREENIO
              MOVE "S  A010  1." TO VDU
              MOVE ELE TO VDUROW
              MOVE SPACES TO LN-INSOURS(I-LEVEL)
              PERFORM SCREENIO
              MOVE "S  M060  2." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO LN-INSEFF-DATE(I-LEVEL) VDUNUM0
              PERFORM SCREENIO
              MOVE "S  M060  3." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO LN-INSEXP-DATE(I-LEVEL) VDUNUM0
              PERFORM SCREENIO
              MOVE "S  A060  4." TO VDU
              MOVE ELE TO VDUROW
              PERFORM SCREENIO
              MOVE "SN N062  5." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO LN-INSCOVR(I-LEVEL) VDUNUM2
              PERFORM SCREENIO
              MOVE "SN N042  6." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO LN-INSPREM(I-LEVEL) VDUNUM2
              PERFORM SCREENIO
              MOVE "SN N042  7." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO LN-INSCOMM(I-LEVEL) VDUNUM2
              PERFORM SCREENIO
              MOVE "S  A010  8." TO VDU
              MOVE ELE TO VDUROW
              MOVE " " TO LN-INSFINCHG(I-LEVEL)
              PERFORM SCREENIO
              GO TO EXIT-ENTER-INSURANCE.

           MOVE VDUNUM0 TO IC-CODE.
           PERFORM OPEN-IC1-FILE.
           PERFORM READ-IC1-FILE.
           PERFORM CLOSE-IC1-FILE.
           IF IO-FG NOT = 0
              MOVE " * INSURANCE COMPANY NOT ON FILE * " TO MESS
              PERFORM SEND-MESS
              GO TO EXIT-ENTER-INSURANCE.
           MOVE VDUNUM0 TO LN-INSCOMP(I-LEVEL).

       ENTER-IC-OURS.
           MOVE "R  A010  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO EXIT-ENTER-INSURANCE.
           IF VDUSTATUS = "1U"
              MOVE 9 TO IO-FG
              GO TO EXIT-ENTER-INSURANCE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-IC-OURS.
           IF NOT (VDUBUF = "Y" OR "N" OR "M")
              MOVE "Y-YES, N-NO OR M-MEMO" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-IC-OURS.
           IF VDUBUF = "N"
              IF LN-SALEFIN NOT = "Y"
                 MOVE "CAN ONLY BE 'N' ON SALES FINANCE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-IC-OURS.
           MOVE VDUBUF TO LN-INSOURS(I-LEVEL).

       ENTER-IC-EFF.
           MOVE "ERNM060  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO EXIT-ENTER-INSURANCE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-OURS.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-IC-EFF.
           MOVE VDU-DATE-YYYYMMDD TO LN-INSEFF-DATE(I-LEVEL).

       ENTER-IC-EXP.
           MOVE "ERNM060  3." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO EXIT-ENTER-INSURANCE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-EFF.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-IC-EXP.
           MOVE VDU-DATE-YYYYMMDD TO LN-INSEXP-DATE(I-LEVEL).

      * SEND TERM:
           MOVE I-LEVEL TO ITRM-SUB.
           PERFORM MISC-INS-TERM.
           MOVE "SN N042  4." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE ITRM-INSTERM TO VDUNUM2.
           PERFORM SCREENIO.

       ENTER-IC-COVR.
           MOVE "ENNN062  5." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO EXIT-ENTER-INSURANCE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-EXP.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-IC-COVR.
           MOVE VDUNUM2 TO LN-INSCOVR(I-LEVEL).
       ENTER-IC-PREM.
           MOVE "ENNN042  6." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO EXIT-ENTER-INSURANCE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-COVR.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-IC-PREM.
           MOVE VDUNUM2 TO LN-INSPREM(I-LEVEL).
           IF BR-AUTO-INS-EARN NOT = "Y"
              MOVE "SN N042  7." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO LN-INSCOMM(I-LEVEL) VDUNUM2
              PERFORM SCREENIO
              GO TO EXIT-ENTER-INSURANCE.
       ENTER-IC-COMM.
           MOVE "ENNN042  7." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO EXIT-ENTER-INSURANCE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-PREM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-IC-COMM.
           MOVE VDUNUM2 TO LN-INSCOMM(I-LEVEL).
       ENTER-IC-FINCHG.
           MOVE "E  A010  8." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO EXIT-ENTER-INSURANCE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-COMM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-IC-FINCHG.
           MOVE VDUBUF TO LN-INSFINCHG(I-LEVEL).
           IF NOT (LN-INSFINCHG(I-LEVEL) = "Y" OR "N")
              MOVE "IS THIS INSURANCE A FINCHG?,  'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-IC-FINCHG.

       EXIT-ENTER-INSURANCE.
           EXIT.

      ***********************************************
       FEE-ENTRY SECTION.
      ***********************************************
           MOVE 0 TO IO-FG.
           IF VDUBUF = "0" OR VDUBUF = SPACES
              MOVE "S  A020  0." TO VDU
              MOVE ELE TO VDUROW
              PERFORM SCREENIO
              MOVE "S  A170  1." TO VDU
              MOVE ELE TO VDUROW
              PERFORM SCREENIO
              MOVE "S  A070  2." TO VDU
              MOVE ELE TO VDUROW
              PERFORM SCREENIO
              MOVE "S  A010  3." TO VDU
              MOVE ELE TO VDUROW
              PERFORM SCREENIO
              MOVE "S  A010  4." TO VDU
              MOVE ELE TO VDUROW
              PERFORM SCREENIO
              MOVE 0 TO LN-FEEAMT(I-LEVEL)
              MOVE " " TO LN-FEECODE(I-LEVEL)
                           LN-FEEOURS(I-LEVEL)
                            LN-FEEFINCHG(I-LEVEL)
              GO TO EXIT-FEE-ENTRY.

           MOVE VDUBUF TO LN-FEECODE(I-LEVEL)
                          CDB-CODE.
           PERFORM GET-CODE.
           IF IO-FG NOT = 0
              MOVE "INVALID FEE CODE " TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
              GO TO EXIT-FEE-ENTRY.
           MOVE "S  A170  1." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CDB-DESC TO VDUBUF.
           PERFORM SCREENIO.

       ENTER-FEEAMT.
           MOVE "ENNN042  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO EXIT-FEE-ENTRY.
           IF VDUSTATUS = "1U"
              MOVE 9 TO IO-FG
              GO TO EXIT-FEE-ENTRY.
           IF VDUSTATUS = "16"
              MOVE "MN/FPMAIN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH
              CANCEL FORM-PROGX.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-FEEAMT.
           MOVE VDUNUM2 TO LN-FEEAMT(I-LEVEL).

           IF ( LN-FEEAMT(I-LEVEL) > CDB-GC-FEE-MAXAMT )
              MOVE "INVALID: AMOUNT IS GREATER THAN THE LIMIT" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FEEAMT.

       ENTER-FEEOURS.
           MOVE "E  A010  3." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO EXIT-FEE-ENTRY.
           IF VDUSTATUS = "1U"
              GO TO ENTER-FEEAMT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-FEEOURS.
           IF VDUBUF NOT = "Y"
                  AND NOT = "N"
                   AND NOT = "M"
              MOVE "Y-YES, N-NO OR M-MEMO" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FEEOURS.
           MOVE VDUBUF TO LN-FEEOURS(I-LEVEL).

       ENTER-FEEFINCHG.
           MOVE "E  A010  4." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO EXIT-FEE-ENTRY.
           IF VDUSTATUS = "1U"
              GO TO ENTER-FEEOURS.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-FEEFINCHG.
           IF NOT (VDUBUF = "Y" OR "N")
              MOVE "IS THIS FEE A FINCHG?  Y-YES OR N-NO?" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FEEFINCHG.
           MOVE VDUBUF TO LN-FEEFINCHG(I-LEVEL).

       EXIT-FEE-ENTRY.
           EXIT.

      *****************************
       WRITE-MODULE SECTION.
      *****************************
           PERFORM OPEN-LN1-FILE.

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

           PERFORM REWRITE-LN1-FILE.

           PERFORM UPDATE-NOTICE-FILE.

       END-WRITE-MODULE.
           PERFORM CLOSE-LN1-FILE.

      ***********************************
       LOG-MAINTENANCE-CHGS SECTION.
      ***********************************

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LN-OWNBR  TO WORK-ERRLOG-BR.
           MOVE LN-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF LNOR-INSURED NOT = LNCH-INSURED
             MOVE "01 "                   TO WORK-ERRLOG-FIELD
             MOVE "INSURED "              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSURED            TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INSURED            TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSTYPES NOT = LNCH-INSTYPES
             MOVE "02 "                   TO WORK-ERRLOG-FIELD
             MOVE "INSTYPES"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSTYPES           TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INSTYPES           TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSELIGIBLE NOT = LNCH-INSELIGIBLE
             MOVE "03 "                   TO WORK-ERRLOG-FIELD
             MOVE "ELIGIBLE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSELIGIBLE        TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INSELIGIBLE        TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOMP(1) NOT = LNCH-INSCOMP(1)
             MOVE "04 "                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOMP1"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOMP(1)         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSCOMP(1)         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSOURS(1) NOT = LNCH-INSOURS(1)
             MOVE "041"                   TO WORK-ERRLOG-FIELD
             MOVE "INSOURS1"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSOURS(1)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INSOURS(1)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSEFF-DATE(1) NOT = LNCH-INSEFF-DATE(1)
             MOVE "042"                   TO WORK-ERRLOG-FIELD
             MOVE "INSEFF 1"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSEFF-DATE(1)     TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSEFF-DATE(1)          TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSEXP-DATE(1) NOT = LNCH-INSEXP-DATE(1)
             MOVE "043"                   TO WORK-ERRLOG-FIELD
             MOVE "INSEXP 1"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSEXP-DATE(1)          TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSEXP-DATE(1)          TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOVR(1) NOT = LNCH-INSCOVR(1)
             MOVE "045"                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOVR1"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOVR(1)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSCOVR(1)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSPREM(1) NOT = LNCH-INSPREM(1)
             MOVE "046"                   TO WORK-ERRLOG-FIELD
             MOVE "INSPREM1"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSPREM(1)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSPREM(1)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOMM(1) NOT = LNCH-INSCOMM(1)
             MOVE "047"                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOMM1"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOMM(1)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSCOMM(1)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSFINCHG(1) NOT = LNCH-INSFINCHG(1)
             MOVE "048"                   TO WORK-ERRLOG-FIELD
             MOVE "INSFIN 1"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSFINCHG(1)       TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INSFINCHG(1)       TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOMP(2) NOT = LNCH-INSCOMP(2)
             MOVE "05 "                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOMP2"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOMP(2)         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSCOMP(2)         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSOURS(2) NOT = LNCH-INSOURS(2)
             MOVE "051"                   TO WORK-ERRLOG-FIELD
             MOVE "INSOURS2"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSOURS(2)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INSOURS(2)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSEFF-DATE(2) NOT = LNCH-INSEFF-DATE(2)
             MOVE "052"                   TO WORK-ERRLOG-FIELD
             MOVE "INSEFF 2"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSEFF-DATE(2)          TO DATE-YYYYMMDD        
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSEFF-DATE(2)          TO DATE-YYYYMMDD         
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSEXP-DATE(2) NOT = LNCH-INSEXP-DATE(2)
             MOVE "053"                   TO WORK-ERRLOG-FIELD
             MOVE "INSEXP 2"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSEXP-DATE(2)          TO DATE-YYYYMMDD     
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSEXP-DATE(2)          TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOVR(2) NOT = LNCH-INSCOVR(2)
             MOVE "055"                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOVR2"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOVR(2)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSCOVR(2)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSPREM(2) NOT = LNCH-INSPREM(2)
             MOVE "056"                   TO WORK-ERRLOG-FIELD
             MOVE "INSPREM2"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSPREM(2)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSPREM(2)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOMM(2) NOT = LNCH-INSCOMM(2)
             MOVE "057"                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOMM2"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOMM(2)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSCOMM(2)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSFINCHG(2) NOT = LNCH-INSFINCHG(2)
             MOVE "058"                   TO WORK-ERRLOG-FIELD
             MOVE "INSFIN 2"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSFINCHG(2)       TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INSFINCHG(2)       TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOMP(3) NOT = LNCH-INSCOMP(3)
             MOVE "06 "                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOMP1"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOMP(3)         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSCOMP(3)         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSOURS(3) NOT = LNCH-INSOURS(3)
             MOVE "061"                   TO WORK-ERRLOG-FIELD
             MOVE "INSOURS3"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSOURS(3)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INSOURS(3)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSEFF-DATE(3) NOT = LNCH-INSEFF-DATE(3)
             MOVE "062"                   TO WORK-ERRLOG-FIELD
             MOVE "INSEFF 3"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSEFF-DATE(3)          TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSEFF-DATE(3)          TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSEXP-DATE(1) NOT = LNCH-INSEXP-DATE(3)
             MOVE "063"                   TO WORK-ERRLOG-FIELD
             MOVE "INSEXP 3"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSEXP-DATE(3)          TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSEXP-DATE(3)          TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOVR(3) NOT = LNCH-INSCOVR(3)
             MOVE "065"                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOVR3"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOVR(3)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSCOVR(3)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSPREM(3) NOT = LNCH-INSPREM(3)
             MOVE "066"                   TO WORK-ERRLOG-FIELD
             MOVE "INSPREM3"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSPREM(3)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSPREM(3)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOMM(3) NOT = LNCH-INSCOMM(3)
             MOVE "067"                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOMM3"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOMM(3)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSCOMM(3)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSFINCHG(3) NOT = LNCH-INSFINCHG(3)
             MOVE "068"                   TO WORK-ERRLOG-FIELD
             MOVE "INSFIN 3"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSFINCHG(3)       TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INSFINCHG(3)       TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOMP(4) NOT = LNCH-INSCOMP(4)
             MOVE "07 "                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOMP4"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOMP(4)         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSCOMP(4)         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSOURS(4) NOT = LNCH-INSOURS(4)
             MOVE "071"                   TO WORK-ERRLOG-FIELD
             MOVE "INSOURS4"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSOURS(4)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INSOURS(4)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSEFF-DATE(4) NOT = LNCH-INSEFF-DATE(4)
             MOVE "072"                   TO WORK-ERRLOG-FIELD
             MOVE "INSEFF 4"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSEFF-DATE(4)          TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSEFF-DATE(4)          TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSEXP-DATE(4) NOT = LNCH-INSEXP-DATE(4)
             MOVE "073"                   TO WORK-ERRLOG-FIELD
             MOVE "INSEXP 4"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSEXP-DATE(4)          TO DATE-YYYYMMDD     
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSEXP-DATE(4)          TO DATE-YYYYMMDD     
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOVR(4) NOT = LNCH-INSCOVR(4)
             MOVE "075"                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOVR4"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOVR(4)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSCOVR(4)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSPREM(4) NOT = LNCH-INSPREM(4)
             MOVE "076"                   TO WORK-ERRLOG-FIELD
             MOVE "INSPREM4"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSPREM(4)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSPREM(4)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOMM(4) NOT = LNCH-INSCOMM(4)
             MOVE "077"                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOMM4"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOMM(4)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSCOMM(4)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSFINCHG(4) NOT = LNCH-INSFINCHG(4)
             MOVE "078"                   TO WORK-ERRLOG-FIELD
             MOVE "INSFIN 4"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSFINCHG(4)       TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INSFINCHG(4)       TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOMP(5) NOT = LNCH-INSCOMP(5)
             MOVE "08 "                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOMP5"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOMP(5)         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSCOMP(5)         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSOURS(5) NOT = LNCH-INSOURS(5)
             MOVE "081"                   TO WORK-ERRLOG-FIELD
             MOVE "INSOURS5"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSOURS(5)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INSOURS(5)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSEFF-DATE(5) NOT = LNCH-INSEFF-DATE(5)
             MOVE "082"                   TO WORK-ERRLOG-FIELD
             MOVE "INSEFF 5"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSEFF-DATE(5)          TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-CHG-N0
             MOVE LNCH-INSEFF-DATE(5)          TO DATE-YYYYMMDD        
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSEXP-DATE(5) NOT = LNCH-INSEXP-DATE(5)
             MOVE "083"                   TO WORK-ERRLOG-FIELD
             MOVE "INSEXP 5"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSEXP-DATE(5)          TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSEXP-DATE(5)          TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOVR(5) NOT = LNCH-INSCOVR(5)
             MOVE "085"                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOVR5"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOVR(5)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSCOVR(5)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSPREM(5) NOT = LNCH-INSPREM(5)
             MOVE "086"                   TO WORK-ERRLOG-FIELD
             MOVE "INSPREM5"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSPREM(5)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSPREM(5)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOMM(5) NOT = LNCH-INSCOMM(5)
             MOVE "087"                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOMM5"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOMM(5)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSCOMM(5)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSFINCHG(5) NOT = LNCH-INSFINCHG(5)
             MOVE "088"                   TO WORK-ERRLOG-FIELD
             MOVE "INSFIN 5"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSFINCHG(5)       TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INSFINCHG(5)       TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

      * OTHER 3
           IF LNOR-INSCOMP(6) NOT = LNCH-INSCOMP(6)
             MOVE "09 "                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOMP6"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOMP(6)         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSCOMP(6)         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSOURS(6) NOT = LNCH-INSOURS(6)
             MOVE "091"                   TO WORK-ERRLOG-FIELD
             MOVE "INSOURS6"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSOURS(6)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INSOURS(6)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSEFF-DATE(6) NOT = LNCH-INSEFF-DATE(6)
             MOVE "092"                   TO WORK-ERRLOG-FIELD
             MOVE "INSEFF 6"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSEFF-DATE(6)          TO DATE-YYYYMMDD        
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSEFF-DATE(6)          TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSEXP-DATE(6) NOT = LNCH-INSEXP-DATE(6)
             MOVE "093"                   TO WORK-ERRLOG-FIELD
             MOVE "INSEXP 6"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSEXP-DATE(6)          TO DATE-YYYYMMDD          
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSEXP-DATE(6)          TO DATE-YYYYMMDD     
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOVR(6) NOT = LNCH-INSCOVR(6)
             MOVE "095"                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOVR6"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOVR(6)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSCOVR(6)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSPREM(6) NOT = LNCH-INSPREM(6)
             MOVE "096"                   TO WORK-ERRLOG-FIELD
             MOVE "INSPREM6"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSPREM(6)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSPREM(6)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOMM(6) NOT = LNCH-INSCOMM(6)
             MOVE "097"                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOMM6"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOMM(6)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSCOMM(6)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSFINCHG(6) NOT = LNCH-INSFINCHG(6)
             MOVE "098"                   TO WORK-ERRLOG-FIELD
             MOVE "INSFIN 6"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSFINCHG(6)       TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INSFINCHG(6)       TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

      * OTHER 4
           IF LNOR-INSCOMP(7) NOT = LNCH-INSCOMP(7)
             MOVE "10 "                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOMP7"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOMP(7)         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSCOMP(7)         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSOURS(7) NOT = LNCH-INSOURS(7)
             MOVE "101"                   TO WORK-ERRLOG-FIELD
             MOVE "INSOURS7"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSOURS(7)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INSOURS(7)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSEFF-DATE(7) NOT = LNCH-INSEFF-DATE(7)
             MOVE "102"                   TO WORK-ERRLOG-FIELD
             MOVE "INSEFF 7"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSEFF-DATE(7)          TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSEFF-DATE(7)          TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSEXP-DATE(7) NOT = LNCH-INSEXP-DATE(7)
             MOVE "103"                   TO WORK-ERRLOG-FIELD
             MOVE "INSEXP 7"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSEXP-DATE(7)          TO DATE-YYYYMMDD        
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSEXP-DATE(7)          TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOVR(7) NOT = LNCH-INSCOVR(7)
             MOVE "105"                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOVR7"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOVR(7)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSCOVR(7)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSPREM(7) NOT = LNCH-INSPREM(7)
             MOVE "106"                   TO WORK-ERRLOG-FIELD
             MOVE "INSPREM7"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSPREM(7)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSPREM(7)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOMM(7) NOT = LNCH-INSCOMM(7)
             MOVE "107"                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOMM7"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOMM(7)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSCOMM(7)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSFINCHG(7) NOT = LNCH-INSFINCHG(7)
             MOVE "108"                   TO WORK-ERRLOG-FIELD
             MOVE "INSFIN 7"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSFINCHG(7)       TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INSFINCHG(7)       TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

      * OTHER 5
           IF LNOR-INSCOMP(8) NOT = LNCH-INSCOMP(8)
             MOVE "11 "                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOMP8"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOMP(8)         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSCOMP(8)         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSOURS(8) NOT = LNCH-INSOURS(8)
             MOVE "111"                   TO WORK-ERRLOG-FIELD
             MOVE "INSOURS8"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSOURS(8)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INSOURS(8)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSEFF-DATE(8) NOT = LNCH-INSEFF-DATE(8)
             MOVE "112"                   TO WORK-ERRLOG-FIELD
             MOVE "INSEFF 8"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSEFF-DATE(8)          TO DATE-YYYYMMDD     
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSEFF-DATE(8)          TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSEXP-DATE(8) NOT = LNCH-INSEXP-DATE(8)
             MOVE "113"                   TO WORK-ERRLOG-FIELD
             MOVE "INSEXP 8"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSEXP-DATE(8)          TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INSEXP-DATE(8)          TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY                  TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOVR(8) NOT = LNCH-INSCOVR(8)
             MOVE "115"                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOVR8"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOVR(8)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSCOVR(8)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSPREM(8) NOT = LNCH-INSPREM(8)
             MOVE "116"                   TO WORK-ERRLOG-FIELD
             MOVE "INSPREM8"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSPREM(8)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSPREM(8)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSCOMM(8) NOT = LNCH-INSCOMM(8)
             MOVE "117"                   TO WORK-ERRLOG-FIELD
             MOVE "INSCOMM8"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSCOMM(8)         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSCOMM(8)         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSFINCHG(8) NOT = LNCH-INSFINCHG(8)
             MOVE "118"                   TO WORK-ERRLOG-FIELD
             MOVE "INSFIN 8"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSFINCHG(8)       TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INSFINCHG(8)       TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INS-CONFIRM-DATE NOT = LNCH-INS-CONFIRM-DATE
             MOVE "12 "                   TO WORK-ERRLOG-FIELD
             MOVE "CONFIRM "              TO WORK-ERRLOG-DESC
             MOVE LNOR-INS-CONFIRM-DATE   TO DATE-YYYYMMDD           
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INS-CONFIRM-DATE   TO DATE-YYYYMMDD          
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INSPREM-DLADJ NOT = LNCH-INSPREM-DLADJ
             MOVE "13 "                   TO WORK-ERRLOG-FIELD
             MOVE "INSDLADJ"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INSPREM-DLADJ      TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-INSPREM-DLADJ      TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-O3INS-DLPROC NOT = LNCH-O3INS-DLPROC
             MOVE "131"                   TO WORK-ERRLOG-FIELD
             MOVE "O3DLPROC"              TO WORK-ERRLOG-DESC
             MOVE LNCH-O3INS-DLPROC       TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-O3INS-DLPROC       TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-FEECODE(1) NOT = LNCH-FEECODE(1)
             MOVE "14 "                   TO WORK-ERRLOG-FIELD
             MOVE "FEE CD 1"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEECODE(1)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEECODE(1)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEAMT(1) NOT = LNCH-FEEAMT(1)
             MOVE "141"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE AMT1"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEAMT(1)          TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-FEEAMT(1)          TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEOURS(1) NOT = LNCH-FEEOURS(1)
             MOVE "142"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE OUR1"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEOURS(1)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEEOURS(1)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEFINCHG(1) NOT = LNCH-FEEFINCHG(1)
             MOVE "143"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE FIN1"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEFINCHG(1)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEEFINCHG(1)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-FEECODE(2) NOT = LNCH-FEECODE(2)
             MOVE "15 "                   TO WORK-ERRLOG-FIELD
             MOVE "FEE CD 2"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEECODE(2)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEECODE(2)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEAMT(2) NOT = LNCH-FEEAMT(2)
             MOVE "151"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE AMT2"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEAMT(2)          TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-FEEAMT(2)          TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEOURS(2) NOT = LNCH-FEEOURS(2)
             MOVE "152"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE OUR2"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEOURS(2)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEEOURS(2)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEFINCHG(2) NOT = LNCH-FEEFINCHG(2)
             MOVE "153"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE FIN2"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEFINCHG(2)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEEFINCHG(2)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-FEECODE(3) NOT = LNCH-FEECODE(3)
             MOVE "16 "                   TO WORK-ERRLOG-FIELD
             MOVE "FEE CD 3"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEECODE(3)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEECODE(3)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEAMT(3) NOT = LNCH-FEEAMT(3)
             MOVE "161"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE AMT3"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEAMT(3)          TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-FEEAMT(3)          TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEOURS(3) NOT = LNCH-FEEOURS(3)
             MOVE "162"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE OUR3"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEOURS(3)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEEOURS(3)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEFINCHG(3) NOT = LNCH-FEEFINCHG(3)
             MOVE "163"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE FIN3"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEFINCHG(3)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEEFINCHG(3)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-FEECODE(4) NOT = LNCH-FEECODE(4)
             MOVE "17 "                   TO WORK-ERRLOG-FIELD
             MOVE "FEE CD 4"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEECODE(4)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEECODE(4)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEAMT(4) NOT = LNCH-FEEAMT(4)
             MOVE "171"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE AMT4"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEAMT(4)          TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-FEEAMT(4)          TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEOURS(4) NOT = LNCH-FEEOURS(4)
             MOVE "172"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE OUR4"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEOURS(4)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEEOURS(4)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEFINCHG(4) NOT = LNCH-FEEFINCHG(4)
             MOVE "173"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE FIN4"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEFINCHG(4)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEEFINCHG(4)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-FEECODE(5) NOT = LNCH-FEECODE(5)
             MOVE "18 "                   TO WORK-ERRLOG-FIELD
             MOVE "FEE CD 5"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEECODE(5)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEECODE(5)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEAMT(5) NOT = LNCH-FEEAMT(5)
             MOVE "181"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE AMT5"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEAMT(5)          TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-FEEAMT(5)          TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEOURS(5) NOT = LNCH-FEEOURS(5)
             MOVE "182"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE OUR5"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEOURS(5)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEEOURS(5)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEFINCHG(5) NOT = LNCH-FEEFINCHG(5)
             MOVE "183"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE FIN5"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEFINCHG(5)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEEFINCHG(5)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-FEECODE(6) NOT = LNCH-FEECODE(6)
             MOVE "19 "                   TO WORK-ERRLOG-FIELD
             MOVE "FEE CD 6"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEECODE(6)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEECODE(6)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEAMT(6) NOT = LNCH-FEEAMT(6)
             MOVE "191"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE AMT6"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEAMT(6)          TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-FEEAMT(6)          TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEOURS(6) NOT = LNCH-FEEOURS(6)
             MOVE "192"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE OUR6"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEOURS(6)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEEOURS(6)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEFINCHG(6) NOT = LNCH-FEEFINCHG(6)
             MOVE "193"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE FIN6"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEFINCHG(6)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEEFINCHG(6)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEECODE(7) NOT = LNCH-FEECODE(7)
             MOVE "20 "                   TO WORK-ERRLOG-FIELD
             MOVE "FEE CD 7"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEECODE(7)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEECODE(7)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEAMT(7) NOT = LNCH-FEEAMT(7)
             MOVE "201"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE AMT7"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEAMT(7)          TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-FEEAMT(7)          TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEOURS(7) NOT = LNCH-FEEOURS(7)
             MOVE "202"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE OUR7"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEOURS(7)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEEOURS(7)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEFINCHG(7) NOT = LNCH-FEEFINCHG(7)
             MOVE "203"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE FIN7"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEFINCHG(7)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEEFINCHG(7)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-FEECODE(8) NOT = LNCH-FEECODE(8)
             MOVE "21 "                   TO WORK-ERRLOG-FIELD
             MOVE "FEE CD 8"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEECODE(8)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEECODE(8)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEAMT(8) NOT = LNCH-FEEAMT(8)
             MOVE "211"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE AMT8"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEAMT(8)          TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-FEEAMT(8)          TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEOURS(8) NOT = LNCH-FEEOURS(8)
             MOVE "212"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE OUR8"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEOURS(8)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEEOURS(8)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEFINCHG(8) NOT = LNCH-FEEFINCHG(8)
             MOVE "213"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE FIN8"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEFINCHG(8)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEEFINCHG(8)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEECODE(9) NOT = LNCH-FEECODE(9)
             MOVE "22 "                   TO WORK-ERRLOG-FIELD
             MOVE "FEE CD 9"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEECODE(9)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEECODE(9)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEAMT(9) NOT = LNCH-FEEAMT(9)
             MOVE "221"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE AMT9"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEAMT(9)          TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-FEEAMT(9)          TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEOURS(9) NOT = LNCH-FEEOURS(9)
             MOVE "222"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE OUR9"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEOURS(9)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEEOURS(9)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEFINCHG(9) NOT = LNCH-FEEFINCHG(9)
             MOVE "223"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE FIN9"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEFINCHG(9)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEEFINCHG(9)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEECODE(10) NOT = LNCH-FEECODE(10)
             MOVE "23 "                   TO WORK-ERRLOG-FIELD
             MOVE "FEE CD 10"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEECODE(10)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEECODE(10)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEAMT(10) NOT = LNCH-FEEAMT(10)
             MOVE "231"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE AMT10"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEAMT(10)          TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-FEEAMT(10)          TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEOURS(10) NOT = LNCH-FEEOURS(10)
             MOVE "232"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE OUR10"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEOURS(10)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEEOURS(10)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-FEEFINCHG(10) NOT = LNCH-FEEFINCHG(10)
             MOVE "233"                   TO WORK-ERRLOG-FIELD
             MOVE "FEE FIN10"              TO WORK-ERRLOG-DESC
             MOVE LNOR-FEEFINCHG(10)         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-FEEFINCHG(10)         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-AD-D-INS-COVR NOT = LNCH-AD-D-INS-COVR
             MOVE "24 "                   TO WORK-ERRLOG-FIELD
             MOVE "AD&D COVR"             TO WORK-ERRLOG-DESC
             MOVE LNOR-AD-D-INS-COVR      TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-AD-D-INS-COVR      TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

      ***********************************
       LOG-FIELD-CHANGE SECTION.
      ***********************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

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

      ****************************************************
       DISPLAY-FIELDS SECTION.
      ****************************************************
           MOVE LN-INSURED TO F01.
           MOVE LN-INSTYPES TO F02.
           IF LN-INSTYPES = "A" MOVE "LL, O1" TO F021
           ELSE IF LN-INSTYPES = "B" MOVE "LL" TO F021
           ELSE IF LN-INSTYPES = "C" MOVE "DL" TO F021
           ELSE IF LN-INSTYPES = "D" MOVE "A&H" TO F021
           ELSE IF LN-INSTYPES = "E" MOVE "PROP" TO F021
           ELSE IF LN-INSTYPES = "F" MOVE "LL, AH" TO F021
           ELSE IF LN-INSTYPES = "G" MOVE "DL, AH" TO F021
           ELSE IF LN-INSTYPES = "H" MOVE "LL, PP" TO F021
           ELSE IF LN-INSTYPES = "I" MOVE "DL, O1" TO F021
           ELSE IF LN-INSTYPES = "J" MOVE "DL, PP" TO F021
           ELSE IF LN-INSTYPES = "K" MOVE "AH, PP" TO F021
           ELSE IF LN-INSTYPES = "L" MOVE "LL, AH, PP" TO F021
           ELSE IF LN-INSTYPES = "M" MOVE "DL, AH, PP" TO F021
           ELSE IF LN-INSTYPES = "N" MOVE "NONE" TO F021
           ELSE IF LN-INSTYPES = "O" MOVE "LL, AH, O1" TO F021
           ELSE IF LN-INSTYPES = "P" MOVE "DL, AH, O1" TO F021
           ELSE IF LN-INSTYPES = "Q" MOVE "AH, O1" TO F021
           ELSE IF LN-INSTYPES = "R" MOVE "PROP2" TO F021
           ELSE IF LN-INSTYPES = "S" MOVE "LL, PP2" TO F021
           ELSE IF LN-INSTYPES = "T" MOVE "DL, PP2" TO F021
           ELSE IF LN-INSTYPES = "U" MOVE "LL, PP, O1" TO F021
           ELSE IF LN-INSTYPES = "V" MOVE "AH, PP2" TO F021
           ELSE IF LN-INSTYPES = "W" MOVE "LL, AH, PP2" TO F021
           ELSE IF LN-INSTYPES = "X" MOVE "DL, AH, PP2" TO F021
           ELSE IF LN-INSTYPES = "Y" MOVE "DL, PP, O1" TO F021
           ELSE IF LN-INSTYPES = "Z" MOVE "PP, O1" TO F021
           ELSE IF LN-INSTYPES = "1" MOVE "O1" TO F021
           ELSE IF LN-INSTYPES = "2" MOVE "LL, PP2, O1" TO F021
           ELSE IF LN-INSTYPES = "3" MOVE "LL, AH, PP, O1" TO F021
           ELSE IF LN-INSTYPES = "4" MOVE "LL, AH, PP2, O1" TO F021
           ELSE IF LN-INSTYPES = "5" MOVE "DL, PP2, O1" TO F021
           ELSE IF LN-INSTYPES = "6" MOVE "DL, AH, PP, O1" TO F021
           ELSE IF LN-INSTYPES = "7" MOVE "DL, AH, PP2, O1" TO F021
           ELSE IF LN-INSTYPES = "8" MOVE "PP2 ,O1" TO F021
           ELSE IF LN-INSTYPES = "9" MOVE "AH, PP, O1" TO F021
           ELSE IF LN-INSTYPES = "0" MOVE "AH, PP2, O1" TO F021.

           MOVE LN-INSELIGIBLE TO F03.
           IF LN-INSELIGIBLE = "A" MOVE "LL, O1" TO F031
           ELSE IF LN-INSELIGIBLE = "B" MOVE "LL" TO F031
           ELSE IF LN-INSELIGIBLE = "C" MOVE "DL" TO F031
           ELSE IF LN-INSELIGIBLE = "D" MOVE "A&H" TO F031
           ELSE IF LN-INSELIGIBLE = "E" MOVE "PROP" TO F031
           ELSE IF LN-INSELIGIBLE = "F" MOVE "LL, AH" TO F031
           ELSE IF LN-INSELIGIBLE = "G" MOVE "DL, AH" TO F031
           ELSE IF LN-INSELIGIBLE = "H" MOVE "LL, PP" TO F031
           ELSE IF LN-INSELIGIBLE = "I" MOVE "DL, O1" TO F031
           ELSE IF LN-INSELIGIBLE = "J" MOVE "DL, PP" TO F031
           ELSE IF LN-INSELIGIBLE = "K" MOVE "AH, PP" TO F031
           ELSE IF LN-INSELIGIBLE = "L" MOVE "LL, AH, PP" TO F031
           ELSE IF LN-INSELIGIBLE = "M" MOVE "DL, AH, PP" TO F031
           ELSE IF LN-INSELIGIBLE = "N" MOVE "NONE" TO F031
           ELSE IF LN-INSELIGIBLE = "O" MOVE "LL, AH, O1" TO F031
           ELSE IF LN-INSELIGIBLE = "P" MOVE "DL, AH, O1" TO F031
           ELSE IF LN-INSELIGIBLE = "Q" MOVE "AH, O1" TO F031
           ELSE IF LN-INSELIGIBLE = "R" MOVE "PROP2" TO F031
           ELSE IF LN-INSELIGIBLE = "S" MOVE "LL, PP2" TO F031
           ELSE IF LN-INSELIGIBLE = "T" MOVE "DL, PP2" TO F031
           ELSE IF LN-INSELIGIBLE = "U" MOVE "LL, PP, O1" TO F031
           ELSE IF LN-INSELIGIBLE = "V" MOVE "AH, PP2" TO F031
           ELSE IF LN-INSELIGIBLE = "W" MOVE "LL, AH, PP2" TO F031
           ELSE IF LN-INSELIGIBLE = "X" MOVE "DL, AH, PP2" TO F031
           ELSE IF LN-INSELIGIBLE = "Y" MOVE "DL, PP, O1" TO F031
           ELSE IF LN-INSELIGIBLE = "Z" MOVE "PP, O1" TO F031
           ELSE IF LN-INSELIGIBLE = "1" MOVE "O1" TO F031
           ELSE IF LN-INSELIGIBLE = "2" MOVE "LL, PP2, O1" TO F031
           ELSE IF LN-INSELIGIBLE = "3" MOVE "LL, AH, PP, O1" TO F031
           ELSE IF LN-INSELIGIBLE = "4" MOVE "LL, AH, PP2, O1" TO F031
           ELSE IF LN-INSELIGIBLE = "5" MOVE "DL, PP2, O1" TO F031
           ELSE IF LN-INSELIGIBLE = "6" MOVE "DL, AH, PP, O1" TO F031
           ELSE IF LN-INSELIGIBLE = "7" MOVE "DL, AH, PP2, O1" TO F031
           ELSE IF LN-INSELIGIBLE = "8" MOVE "PP2 ,O1" TO F031
           ELSE IF LN-INSELIGIBLE = "9" MOVE "AH, PP, O1" TO F031
           ELSE IF LN-INSELIGIBLE = "0" MOVE "AH, PP2, O1" TO F031.

           MOVE LN-INSCOMP(1) TO F04.
           MOVE LN-INSOURS(1) TO F041.
           MOVE LN-INSEFF-DATE(1) TO DATE-YYYYMMDD
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
           MOVE DATE-MMDDYY TO F042.
           MOVE LN-INSEXP-DATE (1) TO DATE-YYYYMMDD
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
           MOVE DATE-MMDDYY TO F043.
           MOVE 1 TO ITRM-SUB.
           PERFORM MISC-INS-TERM.
           MOVE ITRM-INSTERM  TO F044.
           MOVE LN-INSCOVR(1) TO F045.
           MOVE LN-INSPREM(1) TO F046.
           MOVE LN-INSCOMM(1) TO F047.
           MOVE LN-INSFINCHG(1) TO F048.

           MOVE LN-INSCOMP(2) TO F05.
           MOVE LN-INSOURS(2) TO F051.
           MOVE LN-INSEFF-DATE (2) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO F052.
           MOVE LN-INSEXP-DATE (2) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO F053.
           MOVE 2 TO ITRM-SUB.
           PERFORM MISC-INS-TERM.
           MOVE ITRM-INSTERM  TO F054.
           MOVE LN-INSCOVR(2) TO F055.
           MOVE LN-INSPREM(2) TO F056.
           MOVE LN-INSCOMM(2) TO F057.
           MOVE LN-INSFINCHG(2) TO F058.

           MOVE LN-INSCOMP(3) TO F06.
           MOVE LN-INSOURS(3) TO F061.
           MOVE LN-INSEFF-DATE (3) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO F062.
           MOVE LN-INSEXP-DATE (3) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO F063.
           MOVE 3 TO ITRM-SUB.
           PERFORM MISC-INS-TERM.
           MOVE ITRM-INSTERM  TO F064.
           MOVE LN-INSCOVR(3) TO F065.
           MOVE LN-INSPREM(3) TO F066.
           MOVE LN-INSCOMM(3) TO F067.
           MOVE LN-INSFINCHG(3) TO F068.

           MOVE LN-INSCOMP(4) TO F07.
           MOVE LN-INSOURS(4) TO F071.
           MOVE LN-INSEFF-DATE (4) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO F072.
           MOVE LN-INSEXP-DATE (4) TO DATE-MMDDYY.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO F073.
           MOVE 4 TO ITRM-SUB.
           PERFORM MISC-INS-TERM.
           MOVE ITRM-INSTERM  TO F074.
           MOVE LN-INSCOVR(4) TO F075.
           MOVE LN-INSPREM(4) TO F076.
           MOVE LN-INSCOMM(4) TO F077.
           MOVE LN-INSFINCHG(4) TO F078.

           MOVE LN-INSCOMP(5) TO F08.
           MOVE LN-INSOURS(5) TO F081.
           MOVE LN-INSEFF-DATE (5) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO F082.
           MOVE LN-INSEXP-DATE (5) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO F083.
           MOVE 5 TO ITRM-SUB.
           PERFORM MISC-INS-TERM.
           MOVE ITRM-INSTERM  TO F084.
           MOVE LN-INSCOVR(5) TO F085.
           MOVE LN-INSPREM(5) TO F086.
           MOVE LN-INSCOMM(5) TO F087.
           MOVE LN-INSFINCHG(5) TO F088.

           MOVE LN-INSCOMP(6) TO F09.
           MOVE LN-INSOURS(6) TO F091.
           MOVE LN-INSEFF-DATE (6) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO F092.
           MOVE LN-INSEXP-DATE (6) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO F093.
           MOVE 6 TO ITRM-SUB.
           PERFORM MISC-INS-TERM.
           MOVE ITRM-INSTERM  TO F094.
           MOVE LN-INSCOVR(6) TO F095.
           MOVE LN-INSPREM(6) TO F096.
           MOVE LN-INSCOMM(6) TO F097.
           MOVE LN-INSFINCHG(6) TO F098.

           MOVE LN-INSCOMP(7) TO F10.
           MOVE LN-INSOURS(7) TO F101.
           MOVE LN-INSEFF-DATE (7) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO F102.
           MOVE LN-INSEXP-DATE (7) TO DATE-YYYYMMDD
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-YYYYMMDD      TO F103.
           MOVE 7 TO ITRM-SUB.
           PERFORM MISC-INS-TERM.
           MOVE ITRM-INSTERM  TO F104.
           MOVE LN-INSCOVR(7) TO F105.
           MOVE LN-INSPREM(7) TO F106.
           MOVE LN-INSCOMM(7) TO F107.
           MOVE LN-INSFINCHG(7) TO F108.

           MOVE LN-INSCOMP(8) TO F11.
           MOVE LN-INSOURS(8) TO F111.
           MOVE LN-INSEFF-DATE (8) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO F112.
           MOVE LN-INSEXP-DATE (8) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO F113.
           MOVE 8 TO ITRM-SUB.
           PERFORM MISC-INS-TERM.
           MOVE ITRM-INSTERM  TO F114.
           MOVE LN-INSCOVR(8) TO F115.
           MOVE LN-INSPREM(8) TO F116.
           MOVE LN-INSCOMM(8) TO F117.
           MOVE LN-INSFINCHG(8) TO F118.
           MOVE LN-INS-CONFIRM-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F12.

           MOVE LN-INSPREM-DLADJ TO F13.
           MOVE LN-O3INS-DLPROC TO F131.

           PERFORM DISPLAY-FEES
                VARYING ELE FROM 1 BY 1
                   UNTIL ELE > 10.

           MOVE LN-AD-D-INS-COVR TO F24.

           MOVE LN-ORGBR TO WS-ORGBR.

           STRING LN-ORGST, "-", WS-ORGBR, "-", LN-SPRCLASS, "(",
                  LN-CLASS, ")", "-", LN-SUBCLASS, "-", LN-LAWCODE,
                  "-", LN-SMALLOAN, LN-SALEFIN, LN-SERVACCT, LN-LOANTYPE
                  DELIMITED BY "  " INTO FLNSTAT.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.


      ****************************************************
       DISPLAY-FEES SECTION.
      ****************************************************
           MOVE LN-FEECODE(ELE) TO FFEECD(ELE)
                                   CDB-CODE.
           PERFORM GET-CODE.
           IF IO-FG NOT = 0
              MOVE SPACES TO FFEEDESC(ELE)
           ELSE
              MOVE CDB-DESC TO FFEEDESC(ELE).
           MOVE LN-FEEAMT(ELE) TO FFEEAMT(ELE).
           MOVE LN-FEEOURS(ELE) TO FFEEOURS(ELE).
           MOVE LN-FEEFINCHG(ELE) TO FFEEFINCHG(ELE).


      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO LNMAN4-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LNMAN4-SYSDATE.
           DISPLAY LNMAN4-SCREEN UPON LNMAIN-WINDOW-HANDLE.
           MOVE LNMAN4-FORM-FIELDS TO WR-BUF.
           MOVE LNMAN4-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE LNMAN4-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNMAN4-HELP--DIR TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/LNMAN4_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************************
       MISC-PARAGRAPHS SECTION.
      ****************************************************
       COPY "LIBGB/ACCESS.CPY".
      * MUST USE THIS BECAUSE ITRM LOOKS FOR COVR NOT TO BE 0, AND
      * WE DONT HAVE COVR YET
       MISC-INS-TERM.
           MOVE 0 TO ITRM-INSTERM.
           MOVE LN-INSEFF-DATE(ITRM-SUB) TO NUM-DATE.
           MOVE LN-INSEXP-DATE(ITRM-SUB) TO SYS-DATE.
           IF LN-UNITPER-CD = "M" AND LN-UNITPER-FREQ = 1
              PERFORM TIM360
              MOVE ELAPSED-MONTHS TO ITRM-INSTERM
           ELSE
              MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
              PERFORM TIMUPER
              MOVE ELAPSED-UNITPER TO ITRM-INSTERM.
       GET-CODE.
           MOVE "GC"     TO CDB-TYPE.
           MOVE LN-OWNBR TO CDB-BRANCH.
      *--> SET BEFORE     L CDB-CODE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
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
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ****************************************
       IO-ROUTINES SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPIC1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ****************************************
       END-ROUTINE SECTION.
      ****************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LNMAN4-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE LNMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.

