       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PDPG04.
       AUTHOR.        KEC.
       DATE-WRITTEN.  09-12-2000.
      ******************************************************************
      *
      * IDENTIFICATION: MN/PDPG04.C
      *
      * DESCRIPTION   : SCRATCH PAD FILE MAINTENANCE - PAGE 04
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 000912 {PR#00000} PARADATA FINANCIAL [BARB & BETH]
      *             ***NEW*** ADDED PER BARB & BETH.
      *
      *
      * KEC 020624 {PR#01823} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *            ADDED LOGIC FOR USING 'CD-GC-FEE-MAXAMT'
      *
      * MJD 131127 A30 CONVERSION COMPLETED
      *  CS 150814 IF GP-I-AM-EXCEL, TEST THE FEECODE AMOUNT AGAINST CD-
      *            GC-BRANCH-COMM-AMOUNT  EXCEL PR#4821
      *  CS 150923 ADD TEST ON CD-GC-BRANCH-COMM-AMOUNT NOT 202020/020202
      *
      * KEC 2016.0316 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *  CS 170504 ACTIVATE 8 DIGIT DATES
      *
      * KEC 2017.0710 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'PD' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *
      * BAH 190114 GLOBAL PDFILE
      * BAH 01/06/2020 DISPLAY OF INSEFF, INSEXP AND INSTRM WERE WRONG. QUOTES
      *               STORE THE TERM ONLY, NO INSEFF. BUT DOES HAVE INSEXP
      *  CS 2020.01.17 CHANGED SCANKEY ENTRY TO ALPHA FROM NUMERIC
      *                ENTRY IS ID(PIC X),BRANCH,SS
      *  CS 20200120 CHANGED SCANKEY ENTRY TO  "E RA150SCANKEY." TO VDU.
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
           "@(#)MN/PDPG04.CBL S35 02/21/24-13:17:40 >".

      ******************************************************************
      *
      *    F I L E - R E C O R D - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01IC.CPY".
       COPY "LIBLP/LP01IC_SQL.CPY".
       COPY "LIBLP/LPWSIC.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       
       01  CHANGED-PD-REC              PIC X(PD-SIZE)   VALUE SPACES.
       01  ORIGINAL-PD-REC             PIC X(PD-SIZE)   VALUE SPACES.

       01  HOLD-PD1-KEY.
           03  HOLD-PD-ID              PIC X(01)        VALUE SPACES.
           03  HOLD-PD-BRANCH          PIC 9(04)        VALUE ZEROES.
           03  HOLD-PD-SOC-SEC         PIC 9(10)        VALUE ZEROES.
       01  WORK-PD1-KEY.
           03  WORK-PD-ID              PIC X       VALUE SPACES.
           03  WORK-PD-SOC-SEC         PIC 9(10)   VALUE ZEROS.


      ******************************************************************
       01  ONE               PIC S9.
       01  ELE               PIC 99     VALUE 0.
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.
       01  WK-INSURED        PIC X VALUE " ".
           88  GOOD-INSURED  VALUE "N" "M" "S" "J".
       01  I-LEVEL           PIC 99 VALUE 0.

       
      *    'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
       
       01  PAGE-CHANGE-FLAG      PIC X(1)    VALUE "N".
           88  PAGE-CHANGE-NO                VALUE "N".
           88  PAGE-CHANGE-YES               VALUE "Y".

       01  SCANKEY-STAT                PIC X(02)     VALUE SPACES.

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
           03  FILLER     PIC X(28) VALUE
               "230 231 232 233 234 LNSTAT.".

      *-----------------------------------------------------------------
       01  DISPLAY-KEY-BUF                           VALUE SPACES.
           03  D-KEY-ID                PIC X(01).
      *    03  D-KEY-BRANCH            PIC Z(04).
           03  D-KEY-SOC-SEC           PIC Z(10).

      *-----------------------------------------------------------------
       01  DISPLAY-KEY-LIST.
           03  FILLER PIC X(16) VALUE "KEY-ID KEY-SSNO.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       01  MAX            PIC 99   VALUE 23.
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
      * 12. PD-INS-CONFIRMATION
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

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 23 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBMN/PDMAINW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/PDPG04_DEF.CPY".
       COPY "LIBMN/PDPG04_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/PDPG04_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

       MAIN-PROGRAM-LOOP.

           PERFORM ENTER-KEY.

           IF ( IO-TYPE = "END" )
              MOVE "X" TO ERRCD
              GO TO MAIN-PROGRAM-EXIT.

           IF ( IO-TYPE = "CLR" )
              GO TO MAIN-PROGRAM-LOOP.

           PERFORM ENTRY-MODULE.

           IF ( IO-TYPE = "RWR" )
              PERFORM WRITE-MODULE.

           IF ( NEW-REC = "Y" )
              MOVE "P" TO FORM-P1
              MOVE 05  TO FORM-SEL1.

           IF ( FORM-SEL1 NOT = ZEROES ) AND
              ( FORM-P1       = "P"    )
              MOVE PD1-KEY TO SAVEKEY
              MOVE FORM-SEL1 TO FORMSEL
              GO TO MAIN-PROGRAM-EXIT.

           IF ( FORM-SEL2 NOT = ZEROES ) AND
              ( FORM-P2       = "P"    )
              MOVE PD1-KEY TO SAVEKEY
              MOVE FORM-SEL2 TO FORMSEL
              GO TO MAIN-PROGRAM-EXIT.

           GO TO MAIN-PROGRAM-LOOP.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PDPG04" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO PDMAIN-LINK.
           MOVE SPACES TO VDU-PGMENU-SELECTION.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       ENTER-KEY SECTION.
      ******************************************************************
       ENTER-KEY-BEGIN.

           PERFORM FORM-RESET.

           MOVE SPACES TO PD-ID.
           MOVE ZEROES TO PD-BRANCH.
           MOVE ZEROES TO PD-SOC-SEC.

           MOVE ZEROES TO FIRST-SCAN.
           MOVE SPACES TO IO-TYPE.

           IF ( SAVEKEY NOT = SPACES )
              MOVE "Y"     TO PAGE-CHANGE-FLAG
              MOVE SAVEKEY TO PD1-KEY
              MOVE SPACES  TO SAVEKEY
              PERFORM DISPLAY-KEY
              GO TO ENTER-KEY-GET-RECORD.

           MOVE "N"    TO PAGE-CHANGE-FLAG.
           MOVE ZEROES TO NEW-FLAG.

           MOVE EXT-FILPATH-BRNO  TO BR-NO PD-BRANCH HOLD-PD-BRANCH.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
              MOVE "INVALID: BRANCH NUMBER IS NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              MOVE "END" TO IO-TYPE
              GO TO ENTER-KEY-EXIT.

       ENTER-KEY-ID.

           MOVE "E  A010KEY-ID." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-KEY-BEGIN.
           IF (        F6-KEY ) PERFORM ENTER-SCANKEY
                                IF ( STAT-GOOD )
                                   GO TO ENTER-KEY-GET-RECORD
                                ELSE
                                   PERFORM ENTER-KEY-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-KEY-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-KEY-ID.

       
           MOVE VDUBUF TO PD-ID HOLD-PD-ID.

           IF NOT ( PD-ID = "B" OR "L" OR "Q" OR "R" )
              MOVE "INVALID: ONLY (B)ORROWER, (L)OAN," TO MESS
              PERFORM SEND-MESS
              MOVE "(R)ENEWALS OR (Q)UOTES" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEY-ID.

      *-----------------------------------------------------------------
       ENTER-KEY-SSNO.

           MOVE "ENNN100KEY-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-KEY-BEGIN.
           IF (        F6-KEY ) PERFORM ENTER-SCANKEY
                                IF ( STAT-GOOD )
                                   GO TO ENTER-KEY-GET-RECORD
                                ELSE
                                   PERFORM ENTER-KEY-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-KEY-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-KEY-SSNO.

       
           IF ( VDUNUM0 = ZEROES )
              GO TO ENTER-KEY-SSNO.

           MOVE VDUNUM0 TO PD-SOC-SEC HOLD-PD-SOC-SEC.

      *-----------------------------------------------------------------
       ENTER-KEY-GET-RECORD.

           PERFORM OPEN-PD1-FILE.
           PERFORM READ-PD1-FILE.
           PERFORM CLOSE-PD1-FILE.

           IF ( IO-BAD )
              MOVE "* NOT ON FILE *" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEY-BEGIN.

           MOVE ZEROES TO NEW-FLAG.
           MOVE PD-REC TO ORIGINAL-PD-REC.
           PERFORM DISPLAY-FIELDS.

           IF ( NEW-REC = "Y" )
              MOVE 1 TO NEW-FLAG.

           GO TO ENTER-KEY-EXIT.

       ENTER-KEY-END.

           IF ( F7-KEY ) MOVE "END" TO IO-TYPE.

       ENTER-KEY-EXIT.
           EXIT.

      ******************************************************************
       ENTER-SCANKEY SECTION.
      ******************************************************************

           MOVE "XX" TO SCANKEY-STAT.

           MOVE " ENTER LOAN #, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-PD1-FILE.
           MOVE EXT-FILPATH-BRNO TO PD-BRANCH.
           MOVE "B"              TO PD-ID HOLD-PD-ID.

           PERFORM START-PD1-FILE.

       ENTER-SCANKEY-ENTRY.

           MOVE "E RA110SCANKEY." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-SCANKEY-END.
           IF (        F6-KEY ) GO TO ENTER-SCANKEY-READ-NEXT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-SCANKEY-ENTRY.

       
           IF ( VDUBUF = SPACES )
              IF ( FIRST-SCAN NOT = ZEROES )
                 MOVE PD1-KEY TO HOLD-PD1-KEY
                 MOVE PD-REC  TO ORIGINAL-PD-REC
                 MOVE "00"    TO SCANKEY-STAT
                 GO TO ENTER-SCANKEY-END
              ELSE
                 GO TO ENTER-SCANKEY-ENTRY.

           MOVE VDUBUF             TO WORK-PD1-KEY.
           MOVE WORK-PD-ID      TO PD-ID, HOLD-PD-ID.
           MOVE HOLD-PD-BRANCH  TO PD-BRANCH.
           IF ( WORK-PD-SOC-SEC NOT NUMERIC )
              GO TO ENTER-SCANKEY-ENTRY
           ELSE
              MOVE WORK-PD-SOC-SEC TO PD-SOC-SEC HOLD-PD-SOC-SEC.

           PERFORM START-PD1-FILE.

      *-----------------------------------------------------------------
       ENTER-SCANKEY-READ-NEXT.

           PERFORM READ-PD1-FILE-NEXT.
           IF (IO-BAD)
              GO TO ENTER-SCANKEY-END.

           IF PD-BRANCH NOT = PD-PATH-OWNBR
              GO TO ENTER-SCANKEY-READ-NEXT.

      *    UNLOCK PD-FILE.

           PERFORM DISPLAY-KEY.
           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.

           GO TO ENTER-SCANKEY-ENTRY.

       
       ENTER-SCANKEY-END.

           MOVE ZEROES TO FIRST-SCAN.
           PERFORM CLOSE-PD1-FILE.

           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

           MOVE SCANKEY-STAT TO STAT.

       ENTER-SCANKEY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
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
                 A-21 A-22 A-23
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
           MOVE VDUBUF TO PD-INSURED.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUBUF TO PD-INSTYPES.
           IF NOT PD-INSTYPES-VALID
              GO TO ENTER-ELE.
           MOVE "S  A170 " TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 1 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           IF PD-INSTYPES = "A" MOVE "LL, O1" TO VDUBUF
           ELSE IF PD-INSTYPES = "B" MOVE "LL" TO VDUBUF
           ELSE IF PD-INSTYPES = "C" MOVE "DL" TO VDUBUF
           ELSE IF PD-INSTYPES = "D" MOVE "A&H" TO VDUBUF
           ELSE IF PD-INSTYPES = "E" MOVE "PROP" TO VDUBUF
           ELSE IF PD-INSTYPES = "F" MOVE "LL, AH" TO VDUBUF
           ELSE IF PD-INSTYPES = "G" MOVE "DL, AH" TO VDUBUF
           ELSE IF PD-INSTYPES = "H" MOVE "LL, PP" TO VDUBUF
           ELSE IF PD-INSTYPES = "I" MOVE "DL, O1" TO VDUBUF
           ELSE IF PD-INSTYPES = "J" MOVE "DL, PP" TO VDUBUF
           ELSE IF PD-INSTYPES = "K" MOVE "AH, PP" TO VDUBUF
           ELSE IF PD-INSTYPES = "L" MOVE "LL, AH, PP" TO VDUBUF
           ELSE IF PD-INSTYPES = "M" MOVE "DL, AH, PP" TO VDUBUF
           ELSE IF PD-INSTYPES = "N" MOVE "NONE" TO VDUBUF
           ELSE IF PD-INSTYPES = "O" MOVE "LL, AH, O1" TO VDUBUF
           ELSE IF PD-INSTYPES = "P" MOVE "DL, AH, O1" TO VDUBUF
           ELSE IF PD-INSTYPES = "Q" MOVE "AH, O1" TO VDUBUF
           ELSE IF PD-INSTYPES = "R" MOVE "PROP2" TO VDUBUF
           ELSE IF PD-INSTYPES = "S" MOVE "LL, PP2" TO VDUBUF
           ELSE IF PD-INSTYPES = "T" MOVE "DL, PP2" TO VDUBUF
           ELSE IF PD-INSTYPES = "U" MOVE "LL, PP, O1" TO VDUBUF
           ELSE IF PD-INSTYPES = "V" MOVE "AH, PP2" TO VDUBUF
           ELSE IF PD-INSTYPES = "W" MOVE "LL, AH, PP2" TO VDUBUF
           ELSE IF PD-INSTYPES = "X" MOVE "DL, AH, PP2" TO VDUBUF
           ELSE IF PD-INSTYPES = "Y" MOVE "DL, PP, O1" TO VDUBUF
           ELSE IF PD-INSTYPES = "Z" MOVE "PP, O1" TO VDUBUF
           ELSE IF PD-INSTYPES = "1" MOVE "O1" TO VDUBUF
           ELSE IF PD-INSTYPES = "2" MOVE "LL, PP2, O1" TO VDUBUF
           ELSE IF PD-INSTYPES = "3" MOVE "LL, AH, PP, O1" TO VDUBUF
           ELSE IF PD-INSTYPES = "4" MOVE "LL, AH, PP2, O1" TO VDUBUF
           ELSE IF PD-INSTYPES = "5" MOVE "DL, PP2, O1" TO VDUBUF
           ELSE IF PD-INSTYPES = "6" MOVE "DL, AH, PP, O1" TO VDUBUF
           ELSE IF PD-INSTYPES = "7" MOVE "DL, AH, PP2, O1" TO VDUBUF
           ELSE IF PD-INSTYPES = "8" MOVE "PP2 ,O1" TO VDUBUF
           ELSE IF PD-INSTYPES = "9" MOVE "AH, PP, O1" TO VDUBUF
           ELSE IF PD-INSTYPES = "0" MOVE "AH, PP2, O1" TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUBUF TO PD-INSELIGIBLE.
           IF NOT PD-INSELIGIBLE-VALID
              GO TO ENTER-ELE.
           MOVE "S  A170 " TO VDU
           MOVE ELE TO VDUROW.
           MOVE 1 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           IF PD-INSELIGIBLE = "A" MOVE "LL, O1" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "B" MOVE "LL" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "C" MOVE "DL" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "D" MOVE "A&H" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "E" MOVE "PROP" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "F" MOVE "LL, AH" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "G" MOVE "DL, AH" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "H" MOVE "LL, PP" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "I" MOVE "DL, O1" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "J" MOVE "DL, PP" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "K" MOVE "AH, PP" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "L" MOVE "LL, AH, PP" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "M" MOVE "DL, AH, PP" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "N" MOVE "NONE" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "O" MOVE "LL, AH, O1" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "P" MOVE "DL, AH, O1" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "Q" MOVE "AH, O1" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "R" MOVE "PROP2" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "S" MOVE "LL, PP2" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "T" MOVE "DL, PP2" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "U" MOVE "LL, PP, O1" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "V" MOVE "AH, PP2" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "W" MOVE "LL, AH, PP2" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "X" MOVE "DL, AH, PP2" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "Y" MOVE "DL, PP, O1" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "Z" MOVE "PP, O1" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "1" MOVE "O1" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "2" MOVE "LL, PP2, O1" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "3" MOVE "LL, AH, PP, O1" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "4" MOVE "LL, AH, PP2, O1" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "5" MOVE "DL, PP2, O1" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "6" MOVE "DL, AH, PP, O1" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "7" MOVE "DL, AH, PP2, O1" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "8" MOVE "PP2 ,O1" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "9" MOVE "AH, PP, O1" TO VDUBUF
           ELSE IF PD-INSELIGIBLE = "0" MOVE "AH, PP2, O1" TO VDUBUF.
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
           MOVE VDU-DATE-YYYYMMDD TO PD-INS-CONFIRM-DATE.
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDUNUM2 TO PD-INSPREM-DLADJ.
           IF PD-INSPREM-DLADJ NOT = 0
              IF PD-SALEFIN = "N"
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
           MOVE VDUNUM2  TO PD-O3INS-DLPROC.
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

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ***************************************
       INSURANCE-ENTRY SECTION.
      ***************************************
           MOVE 0 TO IO-FG.
           IF VDUNUM0 = 0
              MOVE "SN N020  0." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO PD-INSCOMP(I-LEVEL) VDUNUM0
              PERFORM SCREENIO
              MOVE "S  A010  1." TO VDU
              MOVE ELE TO VDUROW
              MOVE SPACES TO PD-INSOURS(I-LEVEL)
              PERFORM SCREENIO
              MOVE "S  M060  2." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO PD-INSEFF-DATE(I-LEVEL) VDUNUM0
              PERFORM SCREENIO
              MOVE "S  M060  3." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO PD-INSEXP-DATE(I-LEVEL) VDUNUM0
              PERFORM SCREENIO
              MOVE "S  A060  4." TO VDU
              MOVE ELE TO VDUROW
              PERFORM SCREENIO
              MOVE "SN N062  5." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO PD-INSCOVR(I-LEVEL) VDUNUM2
              PERFORM SCREENIO
              MOVE "SN N042  6." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO PD-INSPREM(I-LEVEL) VDUNUM2
              PERFORM SCREENIO
              MOVE "SN N042  7." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO PD-INSCOMM(I-LEVEL) VDUNUM2
              PERFORM SCREENIO
              MOVE "S  A010  8." TO VDU
              MOVE ELE TO VDUROW
              MOVE " " TO PD-INSFINCHG(I-LEVEL)
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
           MOVE VDUNUM0 TO PD-INSCOMP(I-LEVEL).

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
              IF PD-SALEFIN NOT = "Y"
                 MOVE "CAN ONLY BE 'N' ON SALES FINANCE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-IC-OURS.
           MOVE VDUBUF TO PD-INSOURS(I-LEVEL).

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
           MOVE VDU-DATE-YYYYMMDD TO PD-INSEFF-DATE(I-LEVEL).

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
           MOVE VDU-DATE-YYYYMMDD TO PD-INSEXP-DATE(I-LEVEL).

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
           MOVE VDUNUM2 TO PD-INSCOVR(I-LEVEL).
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
           MOVE VDUNUM2 TO PD-INSPREM(I-LEVEL).
           IF BR-AUTO-INS-EARN NOT = "Y"
              MOVE "SN N042  7." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO PD-INSCOMM(I-LEVEL) VDUNUM2
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
           MOVE VDUNUM2 TO PD-INSCOMM(I-LEVEL).
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
           MOVE VDUBUF TO PD-INSFINCHG(I-LEVEL).
           IF NOT (PD-INSFINCHG(I-LEVEL) = "Y" OR "N")
              MOVE "IS THIS INSURANCE A FINCHG?,  'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-IC-FINCHG.

       EXIT-ENTER-INSURANCE.
           EXIT.

      ***********************************
       FEE-ENTRY SECTION.
      ***********************************
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
              MOVE 0 TO PD-FEEAMT(I-LEVEL)
              MOVE " " TO PD-FEECODE(I-LEVEL)
                           PD-FEEOURS(I-LEVEL)
                            PD-FEEFINCHG(I-LEVEL)
              GO TO EXIT-FEE-ENTRY.

           MOVE VDUBUF TO PD-FEECODE(I-LEVEL)
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
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-FEEAMT.
           MOVE VDUNUM2 TO PD-FEEAMT(I-LEVEL).

           IF ( PD-FEEAMT(I-LEVEL) > CDB-GC-FEE-MAXAMT )
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
           MOVE VDUBUF TO PD-FEEOURS(I-LEVEL).

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
           MOVE VDUBUF TO PD-FEEFINCHG(I-LEVEL).

       EXIT-FEE-ENTRY.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-PD1-FILE.

           MOVE PD-REC TO CHANGED-PD-REC.

           IF ( ORIGINAL-PD-REC = CHANGED-PD-REC )
              GO TO WRITE-MODULE-END.

           PERFORM READ-PD1-FILE.
           IF ( IO-BAD )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO WRITE-MODULE-END.

           IF ( PD-REC NOT = ORIGINAL-PD-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO WRITE-MODULE-END.

           MOVE CHANGED-PD-REC TO PD-REC.
           PERFORM REWRITE-PD1-FILE.

       
           MOVE "C"             TO ERRLOGW-ACTION.
           MOVE PD-ID           TO HOLD-PD-ID.
           MOVE PD-BRANCH       TO HOLD-PD-BRANCH.
           MOVE PD-SOC-SEC      TO HOLD-PD-SOC-SEC.
           MOVE HOLD-PD1-KEY TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME   TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       
       WRITE-MODULE-END.

           PERFORM CLOSE-PD1-FILE.

       WRITE-MODULE-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

           MOVE PD-INSURED TO F01.
           MOVE PD-INSTYPES TO F02.
           IF PD-INSTYPES = "A" MOVE "LL, O1" TO F021
           ELSE IF PD-INSTYPES = "B" MOVE "LL" TO F021
           ELSE IF PD-INSTYPES = "C" MOVE "DL" TO F021
           ELSE IF PD-INSTYPES = "D" MOVE "A&H" TO F021
           ELSE IF PD-INSTYPES = "E" MOVE "PROP" TO F021
           ELSE IF PD-INSTYPES = "F" MOVE "LL, AH" TO F021
           ELSE IF PD-INSTYPES = "G" MOVE "DL, AH" TO F021
           ELSE IF PD-INSTYPES = "H" MOVE "LL, PP" TO F021
           ELSE IF PD-INSTYPES = "I" MOVE "DL, O1" TO F021
           ELSE IF PD-INSTYPES = "J" MOVE "DL, PP" TO F021
           ELSE IF PD-INSTYPES = "K" MOVE "AH, PP" TO F021
           ELSE IF PD-INSTYPES = "L" MOVE "LL, AH, PP" TO F021
           ELSE IF PD-INSTYPES = "M" MOVE "DL, AH, PP" TO F021
           ELSE IF PD-INSTYPES = "N" MOVE "NONE" TO F021
           ELSE IF PD-INSTYPES = "O" MOVE "LL, AH, O1" TO F021
           ELSE IF PD-INSTYPES = "P" MOVE "DL, AH, O1" TO F021
           ELSE IF PD-INSTYPES = "Q" MOVE "AH, O1" TO F021
           ELSE IF PD-INSTYPES = "R" MOVE "PROP2" TO F021
           ELSE IF PD-INSTYPES = "S" MOVE "LL, PP2" TO F021
           ELSE IF PD-INSTYPES = "T" MOVE "DL, PP2" TO F021
           ELSE IF PD-INSTYPES = "U" MOVE "LL, PP, O1" TO F021
           ELSE IF PD-INSTYPES = "V" MOVE "AH, PP2" TO F021
           ELSE IF PD-INSTYPES = "W" MOVE "LL, AH, PP2" TO F021
           ELSE IF PD-INSTYPES = "X" MOVE "DL, AH, PP2" TO F021
           ELSE IF PD-INSTYPES = "Y" MOVE "DL, PP, O1" TO F021
           ELSE IF PD-INSTYPES = "Z" MOVE "PP, O1" TO F021
           ELSE IF PD-INSTYPES = "1" MOVE "O1" TO F021
           ELSE IF PD-INSTYPES = "2" MOVE "LL, PP2, O1" TO F021
           ELSE IF PD-INSTYPES = "3" MOVE "LL, AH, PP, O1" TO F021
           ELSE IF PD-INSTYPES = "4" MOVE "LL, AH, PP2, O1" TO F021
           ELSE IF PD-INSTYPES = "5" MOVE "DL, PP2, O1" TO F021
           ELSE IF PD-INSTYPES = "6" MOVE "DL, AH, PP, O1" TO F021
           ELSE IF PD-INSTYPES = "7" MOVE "DL, AH, PP2, O1" TO F021
           ELSE IF PD-INSTYPES = "8" MOVE "PP2 ,O1" TO F021
           ELSE IF PD-INSTYPES = "9" MOVE "AH, PP, O1" TO F021
           ELSE IF PD-INSTYPES = "0" MOVE "AH, PP2, O1" TO F021.

           MOVE PD-INSELIGIBLE TO F03.
           IF PD-INSELIGIBLE = "A" MOVE "LL, O1" TO F031
           ELSE IF PD-INSELIGIBLE = "B" MOVE "LL" TO F031
           ELSE IF PD-INSELIGIBLE = "C" MOVE "DL" TO F031
           ELSE IF PD-INSELIGIBLE = "D" MOVE "A&H" TO F031
           ELSE IF PD-INSELIGIBLE = "E" MOVE "PROP" TO F031
           ELSE IF PD-INSELIGIBLE = "F" MOVE "LL, AH" TO F031
           ELSE IF PD-INSELIGIBLE = "G" MOVE "DL, AH" TO F031
           ELSE IF PD-INSELIGIBLE = "H" MOVE "LL, PP" TO F031
           ELSE IF PD-INSELIGIBLE = "I" MOVE "DL, O1" TO F031
           ELSE IF PD-INSELIGIBLE = "J" MOVE "DL, PP" TO F031
           ELSE IF PD-INSELIGIBLE = "K" MOVE "AH, PP" TO F031
           ELSE IF PD-INSELIGIBLE = "L" MOVE "LL, AH, PP" TO F031
           ELSE IF PD-INSELIGIBLE = "M" MOVE "DL, AH, PP" TO F031
           ELSE IF PD-INSELIGIBLE = "N" MOVE "NONE" TO F031
           ELSE IF PD-INSELIGIBLE = "O" MOVE "LL, AH, O1" TO F031
           ELSE IF PD-INSELIGIBLE = "P" MOVE "DL, AH, O1" TO F031
           ELSE IF PD-INSELIGIBLE = "Q" MOVE "AH, O1" TO F031
           ELSE IF PD-INSELIGIBLE = "R" MOVE "PROP2" TO F031
           ELSE IF PD-INSELIGIBLE = "S" MOVE "LL, PP2" TO F031
           ELSE IF PD-INSELIGIBLE = "T" MOVE "DL, PP2" TO F031
           ELSE IF PD-INSELIGIBLE = "U" MOVE "LL, PP, O1" TO F031
           ELSE IF PD-INSELIGIBLE = "V" MOVE "AH, PP2" TO F031
           ELSE IF PD-INSELIGIBLE = "W" MOVE "LL, AH, PP2" TO F031
           ELSE IF PD-INSELIGIBLE = "X" MOVE "DL, AH, PP2" TO F031
           ELSE IF PD-INSELIGIBLE = "Y" MOVE "DL, PP, O1" TO F031
           ELSE IF PD-INSELIGIBLE = "Z" MOVE "PP, O1" TO F031
           ELSE IF PD-INSELIGIBLE = "1" MOVE "O1" TO F031
           ELSE IF PD-INSELIGIBLE = "2" MOVE "LL, PP2, O1" TO F031
           ELSE IF PD-INSELIGIBLE = "3" MOVE "LL, AH, PP, O1" TO F031
           ELSE IF PD-INSELIGIBLE = "4" MOVE "LL, AH, PP2, O1" TO F031
           ELSE IF PD-INSELIGIBLE = "5" MOVE "DL, PP2, O1" TO F031
           ELSE IF PD-INSELIGIBLE = "6" MOVE "DL, AH, PP, O1" TO F031
           ELSE IF PD-INSELIGIBLE = "7" MOVE "DL, AH, PP2, O1" TO F031
           ELSE IF PD-INSELIGIBLE = "8" MOVE "PP2 ,O1" TO F031
           ELSE IF PD-INSELIGIBLE = "9" MOVE "AH, PP, O1" TO F031
           ELSE IF PD-INSELIGIBLE = "0" MOVE "AH, PP2, O1" TO F031.

           MOVE PD-INSCOMP(1) TO F04.
           MOVE PD-INSOURS(1) TO F041.

      * PD-INSEFF-DATE AND PD-INSTERM ARE REDEFINES
      * Q QUOTES HAVE TERM ONLY
           IF PD-INSEFF-DATE(1) > 1000
              MOVE PD-INSEFF-DATE(1) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY       TO F042
              MOVE 1 TO ITRM-SUB
              PERFORM MISC-INS-TERM
              MOVE ITRM-INSTERM  TO F044
           ELSE
              MOVE 0 TO F042
              MOVE PD-INSTRM(1) TO F044.

           MOVE PD-INSEXP-DATE(1) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F043.

           MOVE PD-INSCOVR(1) TO F045.
           MOVE PD-INSPREM(1) TO F046.
           MOVE PD-INSCOMM(1) TO F047.
           MOVE PD-INSFINCHG(1) TO F048.

           MOVE PD-INSCOMP(2) TO F05.
           MOVE PD-INSOURS(2) TO F051.

      * PD-INSEFF-DATE AND PD-INSTERM ARE REDEFINES
      * Q QUOTES HAVE TERM ONLY
           IF PD-INSEFF-DATE(2) > 1000
              MOVE PD-INSEFF-DATE(2) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY       TO F052
              MOVE 2 TO ITRM-SUB
              PERFORM MISC-INS-TERM
              MOVE ITRM-INSTERM  TO F054
           ELSE
              MOVE 0 TO F042
              MOVE PD-INSTRM(2) TO F054.

           MOVE PD-INSEXP-DATE(2) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F053.

           MOVE PD-INSCOVR(2) TO F055.
           MOVE PD-INSPREM(2) TO F056.
           MOVE PD-INSCOMM(2) TO F057.
           MOVE PD-INSFINCHG(2) TO F058.

           MOVE PD-INSCOMP(3) TO F06.
           MOVE PD-INSOURS(3) TO F061.

      * PD-INSEFF-DATE AND PD-INSTERM ARE REDEFINES
      * Q QUOTES HAVE TERM ONLY
           IF PD-INSEFF-DATE(3) > 1000
              MOVE PD-INSEFF-DATE(3) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY       TO F062
              MOVE 3 TO ITRM-SUB
              PERFORM MISC-INS-TERM
              MOVE ITRM-INSTERM  TO F064
           ELSE
              MOVE 0 TO F062
              MOVE PD-INSTRM(3) TO F064.

           MOVE PD-INSEXP-DATE(3) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F063.
          
           MOVE PD-INSCOVR(3) TO F065.
           MOVE PD-INSPREM(3) TO F066.
           MOVE PD-INSCOMM(3) TO F067.
           MOVE PD-INSFINCHG(3) TO F068.

           MOVE PD-INSCOMP(4) TO F07.
           MOVE PD-INSOURS(4) TO F071.

      * PD-INSEFF-DATE AND PD-INSTERM ARE REDEFINES
      * Q QUOTES HAVE TERM ONLY
           IF PD-INSEFF-DATE(4) > 1000
              MOVE PD-INSEFF-DATE(4) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY       TO F072
              MOVE 4 TO ITRM-SUB
              PERFORM MISC-INS-TERM
              MOVE ITRM-INSTERM  TO F074
           ELSE
              MOVE 0 TO F072
              MOVE PD-INSTRM(4) TO F074.

           MOVE PD-INSEXP-DATE(4) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F073.

           MOVE PD-INSCOVR(4) TO F075.
           MOVE PD-INSPREM(4) TO F076.
           MOVE PD-INSCOMM(4) TO F077.
           MOVE PD-INSFINCHG(4) TO F078.

           MOVE PD-INSCOMP(5) TO F08.
           MOVE PD-INSOURS(5) TO F081.

      * PD-INSEFF-DATE AND PD-INSTERM ARE REDEFINES
      * Q QUOTES HAVE TERM ONLY
           IF PD-INSEFF-DATE(5) > 1000
              MOVE PD-INSEFF-DATE(5) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY       TO F082
              MOVE 5 TO ITRM-SUB
              PERFORM MISC-INS-TERM
              MOVE ITRM-INSTERM  TO F084
           ELSE
              MOVE 0 TO F082
              MOVE PD-INSTRM(5) TO F084.

           MOVE PD-INSEXP-DATE(5) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F083.

           MOVE PD-INSCOVR(5) TO F085.
           MOVE PD-INSPREM(5) TO F086.
           MOVE PD-INSCOMM(5) TO F087.
           MOVE PD-INSFINCHG(5) TO F088.

           MOVE PD-INSCOMP(6) TO F09.
           MOVE PD-INSOURS(6) TO F091.

      * PD-INSEFF-DATE AND PD-INSTERM ARE REDEFINES
      * Q QUOTES HAVE TERM ONLY
           IF PD-INSEFF-DATE(6) > 1000
              MOVE PD-INSEFF-DATE(6) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY       TO F092
              MOVE 6 TO ITRM-SUB
              PERFORM MISC-INS-TERM
              MOVE ITRM-INSTERM  TO F094
           ELSE
              MOVE 0 TO F092
              MOVE PD-INSTRM(6) TO F094.

           MOVE PD-INSEXP-DATE(6) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F093.

           MOVE PD-INSCOVR(6) TO F095.
           MOVE PD-INSPREM(6) TO F096.
           MOVE PD-INSCOMM(6) TO F097.
           MOVE PD-INSFINCHG(6) TO F098.

           MOVE PD-INSCOMP(7) TO F10.
           MOVE PD-INSOURS(7) TO F101.

      * PD-INSEFF-DATE AND PD-INSTERM ARE REDEFINES
      * Q QUOTES HAVE TERM ONLY
           IF PD-INSEFF-DATE(7) > 1000
              MOVE PD-INSEFF-DATE(7) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY       TO F102
              MOVE 7 TO ITRM-SUB
              PERFORM MISC-INS-TERM
              MOVE ITRM-INSTERM  TO F104
           ELSE
              MOVE 0 TO F042
              MOVE PD-INSTRM(7) TO F104.

           MOVE PD-INSEXP-DATE(7) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F103.

           MOVE PD-INSCOVR(7) TO F105.
           MOVE PD-INSPREM(7) TO F106.
           MOVE PD-INSCOMM(7) TO F107.
           MOVE PD-INSFINCHG(7) TO F108.

           MOVE PD-INSCOMP(8) TO F11.
           MOVE PD-INSOURS(8) TO F111.

      * PD-INSEFF-DATE AND PD-INSTERM ARE REDEFINES
      * Q QUOTES HAVE TERM ONLY
           IF PD-INSEFF-DATE(8) > 1000
              MOVE PD-INSEFF-DATE(8) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY       TO F112
              MOVE 8 TO ITRM-SUB
              PERFORM MISC-INS-TERM
              MOVE ITRM-INSTERM  TO F114
           ELSE
              MOVE 0 TO F042
              MOVE PD-INSTRM(8) TO F114.

           MOVE PD-INSEXP-DATE(8) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F113.

           MOVE PD-INSCOVR(8) TO F115.
           MOVE PD-INSPREM(8) TO F116.
           MOVE PD-INSCOMM(8) TO F117.
           MOVE PD-INSFINCHG(8) TO F118.

           MOVE PD-INS-CONFIRM-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F12.

           MOVE PD-INSPREM-DLADJ TO F13.
           MOVE PD-O3INS-DLPROC TO F131.

           PERFORM DISPLAY-FEES
                VARYING ELE FROM 1 BY 1
                   UNTIL ELE > 10.

           MOVE PD-ORGBR TO WS-ORGBR.

           STRING PD-ORGST, "-", WS-ORGBR, "-", PD-SPRCLASS, "(",
                  PD-CLASS, ")", "-", PD-SUBCLASS, "-", PD-LAWCODE,
                  "-", PD-SMALLOAN, PD-SALEFIN, PD-SERVACCT, PD-LOANTYPE
                  DELIMITED BY "  " INTO FLNSTAT.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-KEY SECTION.
      ******************************************************************

           MOVE PD-ID      TO D-KEY-ID.
           MOVE PD-SOC-SEC TO D-KEY-SOC-SEC.
       
           MOVE DISPLAY-KEY-BUF TO WR-BUF.
           MOVE DISPLAY-KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       
       DISPLAY-KEY-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-FEES SECTION.
      ******************************************************************
           MOVE PD-FEECODE(ELE) TO FFEECD(ELE) CDB-CODE.
           PERFORM GET-CODE.
           IF IO-FG NOT = 0
              MOVE SPACES TO FFEEDESC(ELE)
           ELSE
              MOVE CDB-DESC TO FFEEDESC(ELE).
           MOVE PD-FEEAMT(ELE) TO FFEEAMT(ELE).
           MOVE PD-FEEOURS(ELE) TO FFEEOURS(ELE).
           MOVE PD-FEEFINCHG(ELE) TO FFEEFINCHG(ELE).

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO PDPG04-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PDPG04-SYSDATE.
           DISPLAY SPACES UPON PDMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY PDPG04-SCREEN UPON PDMAIN-WINDOW-HANDLE.
           MOVE PDPG04-FORM-FIELDS TO WR-BUF.
           MOVE PDPG04-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE PDPG04-HELP--FILE TO HELP-LINK-FILE.
           MOVE PDPG04-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/PDPG04_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      **************************************************************************
       MISC-PARAGRAPHS SECTION.
      **************************************************************************
       COPY "LIBGB/ACCESS.CPY".
      * MUST USE THIS BECAUSE ITRM LOOKS FOR COVR NOT TO BE 0, AND
      * WE DONT HAVE COVR YET
       MISC-INS-TERM.
           MOVE 0 TO ITRM-INSTERM.
           MOVE PD-INSEFF-DATE(ITRM-SUB) TO NUM-DATE.
           MOVE PD-INSEXP-DATE(ITRM-SUB) TO SYS-DATE.
           IF PD-UNITPER-CD = "M" AND PD-UNITPER-FREQ = 1
              PERFORM TIM360
              MOVE ELAPSED-MONTHS TO ITRM-INSTERM
           ELSE
              MOVE PD-UNITPER-CD TO DATER-UNITPER-CD
              MOVE PD-UNITPER-FREQ TO DATER-UNITPER-FREQ
              PERFORM TIMUPER
              MOVE ELAPSED-UNITPER TO ITRM-INSTERM.
       GET-CODE.
           MOVE "GC"     TO CDB-TYPE.
           MOVE PD-OWNBR TO CDB-BRANCH.
      *--> SET BEFORE       CDB-CODE.
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


      *****************************************************
       IO-ROUTINES SECTION.
      *****************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPPD1IN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPIC1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PDPG04-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE PDMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.

