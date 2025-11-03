       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LTMAIN.
      **************************************************************************
      *   DIR :  MN
      *   DESC:  ADDON INSURANCE MAINTENANCE
      *
      * REV:
      *  JTG 082890 ALLOWED LT-TYPE TO BE 1 THRU 5
      *  BAH 083090 COMMENTED OUT 'SEARCH' FOR ACTIVE INS
      *             TOO MANY SITUATIONS...
      *  MJD 060397 ALLOW ENTRY OF 6 AS INSURANCE TYPE ( 6 = O3 )
      *  JTG 061997 PUT UNLOCK LT-FILE AFTER READ-NEXT IN SCAN
      *  MJD 112597 EXPANDED LT-REC TO 144 BYTES (ADDED 30)
      *  BAH 990617 ALLOW ENTRY OF "ST", NEWPORT
      *  BAH 030127 ADDED CHANGES LOG FOR ANY CHANGES...
      *  JTG 030212 ADDONS CANCELLED VIA ADDON MODULE DO INCORRECT ADDITIONAL
      *             REFUND ON THE 'LT' INTEREST
      *
      *             CHANGED VALUES OF LT-INS-REBATE FLAG SUCH THAT WHEN:
      *               LT-INS-REBATE = " "  NO REFUND OR CANCEL OCCURED
      *                             = "Y"  INS. WAS CANCELLED VIA ADDON POSTING
      *                             = "R"  INS. WAS REBATED   VIA TRANS POSTING
      *                                                               MULL #382
      *  MJD 131120 A30 CONVERSION COMPLETED
      *   CS 150723 ADDED-LT-CANCEL-REASON-CODE WHICH IS UPDATED WHEN YOU ARE
      *             K STREET AND ADDON CANCEL IS PERFORMED   KSTREET PR#4770
      *   CS 150826 CHANGED GP-I-AM-KSTREET/MULL TO JUST MULL PER JUDY
      *                                                        PR#4770
      *  BAH 160118 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0316 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *  CS 161208 REMOVED SEQ ENTRY, FIXED SCAN KEY
      *  CS 170427 ACTIVATE 8 DIGIT DATES
      *
      * KEC 2017.0522 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LT' (I) FILE PREFIX FOR WORKING STORAGE FIELDS.
      *  CS 190104 GLOBAL LTIFILE
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
           "@(#)MN/LTMAIN.CBL S35 02/21/24-12:33:10 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01IC.CPY".
       COPY "LIBLP/LP01IC_SQL.CPY".
       COPY "LIBLP/LPWSIC.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".


       01  ERRCD             PIC X VALUE " ".
       01  ONE               PIC S9.
       01  SUB               PIC 99     COMP-3 VALUE 0.
       01  ELE               PIC 99     VALUE 0.
       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X REDEFINES CHNO
                             PIC X(2).
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.
       01  HOLD-LTI1-KEY          VALUE SPACES.
           03  HOLD-LTI-BRNO     PIC 9(4).
           03  HOLD-LTI-ACCTNO   PIC 9(8).
           03  HOLD-LTI-SEQNO    PIC 999.

       01  CHANGES-LTI1-KEY          VALUE SPACES.
           03  CHANGES-LTI-BRNO     PIC 9(4).
           03  CHANGES-LTI-ACCTNO   PIC 9(8).
           03  CHANGES-LTI-SEQNO    PIC 999.

       01  WORK-AMT          PIC S9(7)V99 COMP-3.


      * SCAN-KEY WILL NOT INCLUDE BRANCH, DIDNT WANT TO CHANGE ENTIRE
      * MAINTENANCE
       01  SCAN-KEY            PIC 9(11).
       01  SCAN-KEYX REDEFINES SCAN-KEY.
           03  SCAN-LTI-ACCTNO  PIC 9(8).
           03  SCAN-LTI-SEQNO   PIC 9(3).

       01  WS-ACCTNO         PIC 9(8).

       01  I-LEVEL           PIC 99 VALUE 0.
       01  ORIGINAL-LTI-REC  PIC X(LTI-SIZE) VALUE SPACES.
       01  CHANGED-LTI-REC   PIC X(LTI-SIZE) VALUE SPACES.

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  F01        PIC ZZ/ZZ/ZZ.
           03  F02        PIC Z9.
           03  F03        PIC Z9.
           03  F04        PIC XX.
           03  F05        PIC Z(5).ZZ-.
           03  F06        PIC Z(5).ZZ-.
           03  F061       PIC Z(5).ZZ-.
           03  F07        PIC ZZ.
           03  F071       PIC X.
           03  F072       PIC ZZ/ZZ/ZZ.
           03  F073       PIC ZZ/ZZ/ZZ.
           03  F074       PIC ZZZ.ZZ.
           03  F075       PIC Z(6).ZZ-.
           03  F076       PIC Z(4).ZZ-.
           03  F077       PIC Z(4).ZZ-.
           03  F078       PIC X.
           03  F08        PIC XX.
           03  F081       PIC X(17).
           03  F082       PIC Z(4).ZZ.
           03  F09        PIC XX.
           03  F091       PIC X(17).
           03  F092       PIC Z(4).ZZ.
           03  F10        PIC ZZZ.
           03  F11        PIC ZZ/ZZ/ZZ.
           03  F12        PIC X.
           03  F13        PIC Z(5).ZZ-.
           03  DISPLAY-TABLE  OCCURS 3 TIMES.
               05  F          PIC ZZZZZZ.ZZ-.
               05  F1         PIC ZZZZZZ.ZZ-.
               05  F2         PIC ZZZZZZ.ZZ-.
               05  F3         PIC ZZZZZZ.ZZ-.
               05  F4         PIC ZZZZZZ.ZZ-.
               05  F4-ALP     REDEFINES F4
                              PIC X(10).
               05  F5         PIC ZZZZZZ.ZZ.
           03  F17            PIC X(3).

       01  DISPLAY-LIST.
           03  FILLER     PIC X(28) VALUE
               "010 020 030 040 050 060 061 ".
           03  FILLER     PIC X(36) VALUE
               "070 071 072 073 074 075 076 077 078 ".
           03  FILLER     PIC X(40) VALUE
               "080 081 082 090 091 092 100 110 120 130 ".
           03  FILLER     PIC X(24) VALUE
               "140 141 142 143 144 145 ".
           03  FILLER     PIC X(24) VALUE
               "150 151 152 153 154 155 ".
           03  FILLER     PIC X(24) VALUE
               "160 161 162 163 164 165.".
           03  FILLER     PIC X(4) VALUE
               "170.".
      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       01  MAX            PIC 99   VALUE 17.
       01  SPEC-TABLE.
      * LT-ORIG-1STPAY
           03  FILLER     PIC X(8) VALUE "ERNM060N".
      * LT-PRIOR-NODEF
           03  FILLER     PIC X(8) VALUE "ENNN020N".
      * LT-TYPE
           03  FILLER     PIC X(8) VALUE "ENNN020N".
      *  4. LT-TRCD
           03  FILLER     PIC X(8) VALUE "E  A020N".
      *  5. LT-INS-PAYMNT-INCR
           03  FILLER     PIC X(8) VALUE "EN S052N".
      *  6. LT-INS-TOT-ADDON
           03  FILLER     PIC X(8) VALUE "EN S052N".
      *  7. LT-INSCOMP
           03  FILLER     PIC X(8) VALUE "ENNN020N".
      *  8. LT-INSFEECD(1)
           03  FILLER     PIC X(8) VALUE "E  A020N".
      *  9. LT-INSFEECD(1)
           03  FILLER     PIC X(8) VALUE "E  A020N".
      * 10. LT-ANTICTERM
           03  FILLER     PIC X(8) VALUE "ENNN030N".
      * 11. LT-ORIG-1STPAY
           03  FILLER     PIC X(8) VALUE "ERNM060N".
      * 12. LT-INT-REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 13. LT-INT-LAST-EARNED
           03  FILLER     PIC X(8) VALUE "EN S052N".
      * 14. LT-ANTICERN(1)
           03  FILLER     PIC X(8) VALUE "ENNS062N".
      * 15. LT-ANTICERN(2)
           03  FILLER     PIC X(8) VALUE "ENNS062N".
      * 16. LT-ANTICERN(3)
           03  FILLER     PIC X(8) VALUE "ENNS062N".
      * 17  LT-INS-CANCEL-CD
           03  FILLER     PIC X(8) VALUE "E  A030N".
       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 17 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       01 LTMAIN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/LTMAIN_DEF.CPY".
       COPY "LIBMN/LTMAIN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/LTMAIN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.


      ****************************************
      *      MAIN PROGRAM MODULE
      ****************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

           MOVE "LTMAIN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS LTMAIN-WINDOW-HANDLE.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-LT-MODULE.
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

      *********************************************
       REC-LT-MODULE SECTION.
      *********************************************
       REC-LT-MODULE-NO.

           MOVE 0 TO LTI-BRNO LTI-ACCTNO LTI-SEQNO FIRST-SCAN.
           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO NEW-FLAG.
       REC-LT-NUMBER.
           MOVE "EN N080LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-LT-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-LT-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-LT-NUMBER.
           MOVE VDUNUM0 TO WS-ACCTNO HOLD-LTI-ACCTNO LTI-ACCTNO.
           IF LTI-ACCTNO = 0
              GO TO REC-LT-NUMBER.
       REC-LT-INSEQ.
           MOVE "EN N030INSEQ." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-LT-NUMBER.
           IF VDUSTATUS = "1U"
              GO TO REC-LT-NUMBER.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-LT-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-LT-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-LT-INSEQ.
           IF VDUNUM0 = 0
              GO TO REC-LT-INSEQ.
           MOVE VDUNUM0 TO LTI-SEQNO HOLD-LTI-SEQNO.
     
       READ-LTFILE.
           MOVE LTI-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE "* LOAN NOT ON FILE *" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LT-MODULE-NO.

           MOVE LN-OWNBR TO LTI-BRNO HOLD-LTI-BRNO.

           PERFORM OPEN-LTI1-FILE.
           PERFORM READ-LTI1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           IF IO-FG = 0
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.

           MOVE LTI-REC TO ORIGINAL-LTI-REC.

       END-REC-LT-MODULE.
           EXIT.

      *********************************************
       ENTER-SCANKEY SECTION.
      *********************************************
           MOVE " ENTER LOAN #, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-LTI1-FILE.
           MOVE LTI-PATH-OWNBR TO LTI-BRNO.


           PERFORM START-LTI1-FILE.
        
           MOVE EXT-FILPATH-BRANCH TO LTI-BRNO.
       ENTER-KEY.
           MOVE "EN N110SCANKEY." TO VDU.
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
                 MOVE LTI1-KEY TO HOLD-LTI1-KEY
                 MOVE LTI-REC TO ORIGINAL-LTI-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUNUM0 TO SCAN-KEY.
           MOVE EXT-FILPATH-BRANCH TO LTI-BRNO.
           MOVE SCAN-LTI-ACCTNO    TO LTI-ACCTNO.
           MOVE SCAN-LTI-SEQNO     TO LTI-SEQNO.

           PERFORM START-LTI1-FILE.
       READ-FILES-NEXT.
           PERFORM READ-LTI1-FILE-NEXT.
      *    UNLOCK LTI-FILE.
           IF IO-FG = 9 OR (LTI-BRNO NOT = LTI-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           PERFORM DISPLAY-SCAN-KEY.

           MOVE LTI-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.

           GO TO ENTER-KEY.
       DISPLAY-SCAN-KEY.
           MOVE LTI-ACCTNO TO WS-ACCTNO SCAN-LTI-ACCTNO.
           MOVE "SN N080LNNO." TO VDU.
           MOVE SCAN-LTI-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N030INSEQ." TO VDU.
           MOVE LTI-SEQNO TO VDUNUM0.
           PERFORM SCREENIO.
       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           PERFORM CLOSE-LTI1-FILE.

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

           IF ( ELE = 15 OR 16 )
              GO TO CREATE-ROUTINE.

           IF ELE = 17
              GO TO CREATE-ROUTINE.

           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
      *    IF ACTION = SPACES
      *       MOVE LT-TYPE TO VDU-NUM
      *       PERFORM SEARCH-FOR-ACTIVE-INS
      *       MOVE CHANGED-LTI-REC TO LT-REC
      *       IF ERRCD = "X"
      *          MOVE "LOAN TYPE ALREADY IN EFFECT" TO MESS
      *          PERFORM SEND-MESS
      *          GO TO MODIFY-ROUTINE.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
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

           IF ( ELE = 15 OR 16 )
              GO TO MODIFY-ROUTINE.

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
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO A-01 A-02 A-03 A-04 A-05 A-06 A-07
                 A-08 A-09 A-10 A-11 A-12 A-13 A-14
                 A-17 DEPENDING ON ELE.
           GO TO EXIT-ENTRY-MODULE.


      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           MOVE VDU-DATE-YYYYMMDD  TO LTI-ORIG-1STPYDATE.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUNUM0 TO LTI-PRIOR-NODEF.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUNUM0 TO LTI-INS-TYPE.
           IF LTI-INS-TYPE < 1 OR > 6
              MOVE "MUST BE 1, 2, 3, 4 ,5 OR 6" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

      *    PERFORM SEARCH-FOR-ACTIVE-INS.
      *    MOVE CHANGED-LTI-REC TO LT-REC.
      *    IF ERRCD = "X"
      *       MOVE "LOAN TYPE ALREADY IN EFFECT" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       A-04.
           MOVE VDUBUF TO LTI-TRCD.
           IF LTI-TRCD NOT = "RT" AND
                       NOT = "ET" AND
                        NOT = "LP" AND
                         NOT = "NP" AND
                          NOT = "ST"
              MOVE "MUST BE 'RT' 'ET' 'LP' 'NP' OR 'ST' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       A-05.
           MOVE VDUNUM2 TO LTI-INS-PAYMNT-INCR.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUNUM2 TO LTI-INS-TOT-ADDON.
      * TOTAL FINANCE CHARGE:
       A-061.
           MOVE "ENNS052  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-061.
           MOVE VDUNUM2 TO LTI-ANTICERN(3).
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE 1 TO I-LEVEL.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-08.
           PERFORM FEE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-09.
           PERFORM FEE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUNUM0 TO LTI-ANTICTERM.
           GO TO CREATE-ROUTINE.

       A-11.
           MOVE VDU-DATE-YYYYMMDD  TO LTI-ANTIC-START-DATE.
           GO TO CREATE-ROUTINE.
       A-12.
           IF VDUBUF NOT = "Y" AND
                      NOT = "C" AND
                       NOT = " " AND
                        NOT = "R"
              MOVE "ENTER: 'Y', 'C', 'R', OR ' ' " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LTI-INT-REBATE.
           GO TO CREATE-ROUTINE.

       A-13.
           MOVE VDUNUM2 TO LTI-INT-LAST-EARNED.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDUNUM2 TO LTI-ANTICERN(ELE - 13).

      * ANTICIPATED ADJ:
       A-141.
           MOVE "ENNS062  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-141.
           MOVE VDUNUM2 TO LTI-ANTICADJ(ELE - 13).
           SUBTRACT LTI-ANTICADJ(ELE - 13) FROM LTI-ANTICERN(ELE - 13).

      * NET ANTICIPATED:
           MOVE "SN S062  2." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE LTI-ANTICERN(ELE - 13) TO VDUNUM2.
           PERFORM SCREENIO.

      * ACCERN:
       A-143.
           MOVE "ENNS062  3." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-141.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-143.
           MOVE VDUNUM2 TO LTI-ACCERN(ELE - 13).

      * UNEARNED:
           MOVE "SN S062  4." TO VDU.
           MOVE ELE TO VDUROW.
           IF LN-PLCD = " " AND LN-SERVACCT NOT = "Y"
             SUBTRACT LTI-ACCERN(ELE - 13) FROM LTI-ANTICERN(ELE - 13)
                                              GIVING VDUNUM2
           ELSE
              MOVE 0 TO VDUNUM2.
           PERFORM SCREENIO.

      * UPFRONT EARNINGS:
       A-145.
      * PREPARING FOR NEGATIVE ANTIC
           MOVE "ENNN062  5." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-143.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-145.

           ADD LTI-ANTICERN(ELE - 13) LTI-ANTICADJ(ELE - 13)
               GIVING WORK-AMT.

           IF WORK-AMT > 0
            IF LTI-ANTICADJ(ELE - 13) NOT = 0
             IF VDUNUM2 > WORK-AMT
                MOVE "UPFRONT EARNINGS CAN'T EXCEED ANTICIPATED"
                                                           TO MESS
               PERFORM SEND-MESS
               GO TO A-145.

           IF WORK-AMT > 0
            IF LTI-ANTICADJ(ELE - 13) = 0
             IF VDUNUM2 > LTI-ACCERN(ELE - 13)
                MOVE "UPFRONT EARNINGS MUST BE INCLUDED IN EARNINGS"
                                                           TO MESS
               PERFORM SEND-MESS
               GO TO A-145.
           MOVE VDUNUM2 TO LTI-UFERN(ELE - 13).
       A-17.
           MOVE VDUBUF TO LTI-CANCEL-REASON-CD.
           GO TO CREATE-ROUTINE.

       EXIT-ENTRY-MODULE.
           EXIT.

      ***************************************
       INSURANCE-ENTRY SECTION.
      ***************************************
           MOVE 0 TO IO-FG.
           IF VDUNUM0 = 0
              MOVE "SN N020  0." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO LTI-INSCOMP VDUNUM0
              PERFORM SCREENIO
              MOVE "S  A010  1." TO VDU
              MOVE ELE TO VDUROW
              MOVE " " TO LTI-INSOURS
              PERFORM SCREENIO
              MOVE "S  M060  2." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO LTI-INSEFF-DATE VDUNUM0
              PERFORM SCREENIO
              MOVE "S  M060  3." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO LTI-INSEXP-DATE VDUNUM0
              PERFORM SCREENIO
              MOVE "S  A060  4." TO VDU
              MOVE ELE TO VDUROW
              PERFORM SCREENIO
              MOVE "SN N062  5." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO LTI-INSCOVR VDUNUM2
              PERFORM SCREENIO
              MOVE "SN N042  6." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO LTI-INSPREM VDUNUM2
              PERFORM SCREENIO
              MOVE "SN N042  7." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO LTI-INSCOMM VDUNUM2
              PERFORM SCREENIO
              MOVE "S  A010  8." TO VDU
              MOVE ELE TO VDUROW
              MOVE " " TO LTI-INS-REBATE
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
           MOVE VDUNUM0 TO LTI-INSCOMP.

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
           IF VDUBUF NOT = "Y"
                  AND NOT = "N"
                   AND NOT = "M"
              MOVE "Y-YES, N-NO OR M-MEMO" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-IC-OURS.
           IF VDUBUF = "N"
              IF LN-SALEFIN NOT = "Y"
                 MOVE "CAN ONLY BE 'N' ON SALES FINANCE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-IC-OURS.
           MOVE VDUBUF TO LTI-INSOURS.

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
           MOVE VDU-DATE-YYYYMMDD  TO LTI-INSEFF-DATE.

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
           MOVE VDU-DATE-YYYYMMDD  TO LTI-INSEXP-DATE.

      * SEND TERM:
           MOVE I-LEVEL TO ITRM-SUB.
           PERFORM MISC-INS-TERM.
           MOVE "SN N042  4." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE ITRM-INSTERM TO VDUNUM2.
           PERFORM SCREENIO.

       ENTER-IC-COVR.
           MOVE "ENNS062  5." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO EXIT-ENTER-INSURANCE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-EXP.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-IC-COVR.
           MOVE VDUNUM2 TO LTI-INSCOVR.
       ENTER-IC-PREM.
           MOVE "ENNS042  6." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO EXIT-ENTER-INSURANCE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-COVR.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-IC-PREM.
           MOVE VDUNUM2 TO LTI-INSPREM.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR
           ELSE
              PERFORM CLOSE-BR-FILE.
           IF BR-AUTO-INS-EARN NOT = "Y"
              MOVE "SN S042  7." TO VDU
              MOVE ELE TO VDUROW
              MOVE LTI-INSPREM TO LTI-INSCOMM VDUNUM2
              PERFORM SCREENIO
              GO TO ENTER-IC-REBATE.

       ENTER-IC-COMM.
           MOVE "ENNS042  7." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO EXIT-ENTER-INSURANCE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-PREM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-IC-COMM.
           MOVE VDUNUM2 TO LTI-INSCOMM.

       ENTER-IC-REBATE.
           MOVE "E  A010  8." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 8 TO IO-FG
              GO TO EXIT-ENTER-INSURANCE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-COMM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-IC-REBATE.
           IF NOT (VDUBUF = " " OR "R" OR "Y")
              GO TO ENTER-IC-REBATE.
           MOVE VDUBUF TO LTI-INS-REBATE.

       EXIT-ENTER-INSURANCE.
           EXIT.

      ***********************************
       FEE-ENTRY SECTION.
      ***********************************
           MOVE 0 TO IO-FG.
           IF VDUBUF = "0" OR VDUBUF = " "
              MOVE "S  A020  0." TO VDU
              MOVE ELE TO VDUROW
              PERFORM SCREENIO
              MOVE "S  A170  1." TO VDU
              MOVE ELE TO VDUROW
              PERFORM SCREENIO
              MOVE "S  A070  2." TO VDU
              MOVE ELE TO VDUROW
              PERFORM SCREENIO
              MOVE 0   TO LTI-INSFEE(ELE - 7)
              MOVE " " TO LTI-INSFEECD(ELE - 7)
              GO TO EXIT-FEE-ENTRY.

           MOVE VDUBUF TO LTI-INSFEECD(ELE - 7) CDB-CODE.
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
           MOVE VDUNUM2 TO LTI-INSFEE(ELE - 7).

       EXIT-FEE-ENTRY.
           EXIT.

      ***************************************
       WRITE-MODULE SECTION.
      ***************************************
           PERFORM OPEN-LTI1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE LTI-REC TO CHANGED-LTI-REC.
           IF ORIGINAL-LTI-REC = CHANGED-LTI-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-LTI1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF LTI-REC NOT = ORIGINAL-LTI-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-LTI-REC TO LTI-REC.
           PERFORM REWRITE-LTI1-FILE.

      * CODE TO WRITE CHANGE FILE.
           MOVE LTI-BRNO   TO CHANGES-LTI-BRNO.
           MOVE LTI-ACCTNO TO CHANGES-LTI-ACCTNO.
           MOVE LTI-SEQNO  TO CHANGES-LTI-SEQNO.
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE CHANGES-LTI1-KEY TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-LTI1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-LTI1-FILE.

      **************************************
       DELETE-MODULE SECTION.
      **************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-LTI1-FILE.
           PERFORM READ-LTI1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF LTI-REC NOT = ORIGINAL-LTI-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-LTI1-FILE.
       END-DELETE-MODULE.
           PERFORM CLOSE-LTI1-FILE.

      **************************************
       DISPLAY-FIELDS SECTION.
      **************************************
           MOVE LTI-ORIG-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO F01.

           MOVE LTI-PRIOR-NODEF TO F02.
           MOVE LTI-INS-TYPE TO F03.
           MOVE LTI-TRCD TO F04.
           MOVE LTI-INS-PAYMNT-INCR TO F05.
           MOVE LTI-INS-TOT-ADDON TO F06.
           MOVE LTI-ANTICERN(3) TO F061.
           MOVE LTI-INSCOMP TO F07.
           MOVE LTI-INSOURS TO F071.
           MOVE LTI-INSEFF-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO F072.

           MOVE LTI-INSEXP-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO F073.

           MOVE 1 TO ITRM-SUB.
           PERFORM MISC-INS-TERM.
           MOVE ITRM-INSTERM  TO F074.
           MOVE LTI-INSCOVR    TO F075.
           MOVE LTI-INSPREM    TO F076.
           MOVE LTI-INSCOMM    TO F077.
           MOVE LTI-INS-REBATE TO F078.

           MOVE LTI-INSFEECD(1) TO F08 CDB-CODE.
           PERFORM GET-CODE.
           MOVE CDB-DESC      TO F081.
           MOVE LTI-INSFEE(1) TO F082.

           MOVE LTI-INSFEECD(2) TO F09 CDB-CODE.
           PERFORM GET-CODE.
           MOVE CDB-DESC      TO F091.
           MOVE LTI-INSFEE(2) TO F092.

           MOVE 0 TO SUB.
           PERFORM DISPLAY-FIELDS-COMPONENTS 1 TIMES.

           MOVE LTI-ANTICTERM TO F10.
           MOVE LTI-ANTIC-START-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY          TO F11.

           MOVE LTI-INT-REBATE TO F12.
           MOVE LTI-INT-LAST-EARNED TO F13.
           MOVE LTI-CANCEL-REASON-CD TO F17.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ************************************************
       DISPLAY-FIELDS-COMPONENTS SECTION.
      ************************************************
           ADD 1 TO SUB.
           ADD LTI-ANTICADJ(SUB) LTI-ANTICERN(SUB) GIVING F(SUB).
           MOVE LTI-ANTICADJ(SUB) TO F1(SUB).
           MOVE LTI-ANTICERN(SUB) TO F2(SUB).
           MOVE LTI-ACCERN(SUB) TO F3(SUB).
           IF LN-PLCD = " " AND LN-SERVACCT NOT = "Y"
              SUBTRACT LTI-ACCERN(SUB) FROM LTI-ANTICERN(SUB)
                                              GIVING F4(SUB)
              IF F4-ALP(SUB) NOT = " "
                 PERFORM DISPLAY-FIELDS-COMPONENTS-RB
              ELSE
                 NEXT SENTENCE
           ELSE
              IF SUB = 1
                 IF LN-PLCD NOT = " "
                    MOVE "NONE (PL)" TO F4-ALP(SUB)
                 ELSE
                    IF LN-SERVACCT = "Y"
                       MOVE "NONE (SA)" TO F4-ALP(SUB).
           MOVE LTI-UFERN(SUB) TO F5(SUB).

       DISPLAY-FIELDS-COMPONENTS-RB.
      * SKIP REBATED COMPONENTS:
           IF SUB = 1 AND
              ( LTI-ANTICADJ(1) NOT = 0
                      OR (NOT (LTI-INT-REBATE = " " OR "C")) )
              MOVE "  REBATED?" TO F4-ALP(1)
           ELSE
           IF SUB = 2 AND
              ( LTI-ANTICADJ(2) NOT = 0
                      OR (NOT (LTI-INT-REBATE = " " OR "C")) )
              MOVE "  REBATED?" TO F4-ALP(2)
           ELSE
           IF SUB = 3 AND
              ( LTI-ANTICADJ(3) NOT = 0
                      OR (NOT (LTI-INT-REBATE = " " OR "C")) )
              MOVE "  REBATED?" TO F4-ALP(3).

       DISP-FIELDS-EXIT.
           EXIT.

      ***********************************************
       NEW-RECORD-SETUP SECTION.
      ***********************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000DISP." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-LTI-FILE.

           MOVE HOLD-LTI-BRNO   TO LTI-BRNO.
           MOVE HOLD-LTI-ACCTNO TO LTI-ACCTNO.
           MOVE HOLD-LTI-SEQNO  TO LTI-SEQNO.

           MOVE 0 TO ELE.


      ***************************************************************
       FORM-RESET SECTION.
      ***************************************************************
           MOVE EXT-CONAME-CONAME TO LTMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LTMAIN-SYSDATE.
           DISPLAY SPACES UPON LTMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LTMAIN-SCREEN UPON LTMAIN-WINDOW-HANDLE.
           MOVE LTMAIN-FORM-FIELDS TO WR-BUF.
           MOVE LTMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE LTMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE LTMAIN-HELP--DIR TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/LTMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPLTICL.CPY".

      *********************************************************
       MISC-PARAGRAPHS SECTION.
      *********************************************************
       COPY "LIBGB/ACCESS.CPY".
       GET-CODE.
           MOVE "GC"     TO CDB-TYPE.
           MOVE LN-OWNBR TO CDB-BRANCH.
      *--> SET BEFORE       CDB-CODE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO CDB-REC.

      * MUST USE THIS BECAUSE ITRM LOOKS FOR COVR NOT TO BE 0, AND
      * WE DONT HAVE COVR YET
       MISC-INS-TERM.
           MOVE 0 TO ITRM-INSTERM.
           MOVE LTI-INSEFF-DATE TO NUM-DATE.
           MOVE LTI-INSEXP-DATE TO SYS-DATE.
           PERFORM TIM360.
           MOVE ELAPSED-MONTHS TO ITRM-INSTERM.
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
           MOVE "RECORD HAS ALREADY BEEN CREATED!" TO MESS.
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

      **************************************
       IO-ROUTINES SECTION.
      **************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTI1IN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPIC1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      **************************************
       END-ROUTINE SECTION.
      **************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY LTMAIN-WINDOW-HANDLE
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
