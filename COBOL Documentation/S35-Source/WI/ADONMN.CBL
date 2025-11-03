       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ADONMN.
      *************************************************************************
      *   DIR :  WI
      *   DESC:  ADDON INSURANCE / FEE  MAINTENANCE
      *           CALLED FROM LP/LONIJ0, XONIN0
      *
      *                  (WINDOW VERSION)
      *REV:
      * JTG 070192 ALLOWED LT-TYPE TO BE 1 THRU 5 OR 0
      * MJD 112597 EXPANDED LT-REC TO 144 BYTES (ADDED 30)
      * BAH 990617 ALLOW ENTRY OF "ST" TRCD, NEWPORT, LAURA
      * JTG 030212 ADDONS CANCELLED VIA ADDON MODULE DO INCORRECT ADDITIONAL
      *            REFUND ON THE 'LT' INTEREST
      *
      *            CHANGED VALUES OF LT-INS-REBATE FLAG SUCH THAT WHEN:
      *              LT-INS-REBATE = " "  NO REFUND OR CANCEL OCCURED
      *                            = "Y"  INS. WAS CANCELLED VIA ADDON POSTING
      *                            = "R"  INS. WAS REBATED   VIA TRANS POSTING
      *                                                               MULL #382
      *  CS 150728 ADD LT-CANCEL-REASON-CD KSTREET PR#4770 
      *  CS 150814 IF GP-I-AM-EXCEL, TEST THE FEECODE AMOUNT AGAINST CD-
      *            GC-BRANCH-COMM-AMOUNT  EXCEL PR#4821
      *  CS 150825 CHANGED GP-I-AM-KTREET/MULL TO JUST MULL PR#4770
      *  CS 150922 REMOVED EXCEL TEST ON CDB-GC-BRANCH-COMM, DONT NEED PR#4821
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0318 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CDB-GC-????.
      *
      *  BAH 170405 ACTIVATE 8 DIGIT DATES, PD0006
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
           "@(#)WI/ADONMN.CBL S35 02/22/24-08:23:08 >".
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

       01  ADONMN-LINES              PIC 99   VALUE 22.
       01  ADONMN-COLUMNS            PIC 99   VALUE 78.
       01  ADONMN-BEGIN-Y            PIC 99   VALUE 2.
       01  ADONMN-BEGIN-X            PIC 99   VALUE 2.
       01  ERRCD             PIC X     VALUE SPACES.

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

       01  WORK-AMT          PIC S9(7)V99 COMP-3.
       01  LEGEND            PIC X(70).
       01  LEGEND-LIST       PIC X(8)   VALUE "WSEL:D.".

       01  I-LEVEL           PIC 99 VALUE 0.

       01  ORIGINAL-LTI-REC      PIC X(LTI-SIZE) VALUE SPACES.
       01  CHANGED-LTI-REC       PIC X(LTI-SIZE) VALUE SPACES.
       01  CHAN              PIC X(4).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  F01        PIC Z9.
           03  F02        PIC XX.
           03  F03        PIC ZZ/ZZ/ZZ.
           03  F04        PIC Z9.
           03  F05        PIC Z(5).ZZ-.
           03  F06        PIC Z(5).ZZ-.
           03  F07        PIC Z(5).ZZ-.
           03  F08        PIC ZZ.
           03  F081       PIC X.
           03  F082       PIC ZZ/ZZ/ZZ.
           03  F083       PIC ZZ/ZZ/ZZ.
           03  F084       PIC ZZZ.ZZ.
           03  F085       PIC Z(6).ZZ-.
           03  F086       PIC Z(4).ZZ-.
           03  F087       PIC Z(4).ZZ-.
           03  F088       PIC X.
           03  F09        PIC XX.
           03  F091       PIC X(17).
           03  F092       PIC Z(4).ZZ.
           03  F10        PIC XX.
           03  F101       PIC X(17).
           03  F102       PIC Z(4).ZZ.
           03  F11        PIC ZZZ.
           03  F12        PIC ZZ/ZZ/ZZ.
           03  F13        PIC X.
           03  DISPLAY-TABLE  OCCURS 1 TIMES.
               05  F          PIC ZZZZZZ.ZZ-.
               05  F1         PIC ZZZZZZ.ZZ-.
               05  F2         PIC ZZZZZZ.ZZ-.
               05  F3         PIC ZZZZZZ.ZZ-.
               05  F4         PIC ZZZZZZ.ZZ-.
               05  F4-ALP     REDEFINES F4
                              PIC X(10).
               05  F5         PIC ZZZZZZ.ZZ.
           03  F15        PIC Z(5).ZZ-.
           03  F16        PIC X(3).
       01  DISPLAY-LIST.
           03  FILLER     PIC X(28) VALUE
               "010 020 030 040 050 060 070 ".
           03  FILLER     PIC X(36) VALUE
               "080 081 082 083 084 085 086 087 088 ".
           03  FILLER     PIC X(36) VALUE
               "090 091 092 100 101 102 110 120 130 ".
           03  FILLER     PIC X(24) VALUE
               "140 141 142 143 144 145 ".
           03  FILLER     PIC X(8) VALUE
               "150 160.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       01  MAX            PIC 99   VALUE 16.
       01  SPEC-TABLE.
      *  1. LTI-TYPE
           03  FILLER     PIC X(8) VALUE "ENNN020 ".
      *  2. LTI-TRCD
           03  FILLER     PIC X(8) VALUE "E  A020 ".
      *  3. LTI-ORIG-1STPYDATE
           03  FILLER     PIC X(8) VALUE "E NM060 ".
      *  4. LTI-PRIOR-NODEF
           03  FILLER     PIC X(8) VALUE "ENNN020 ".
      *  5. LTI-INS-PAYMNT-INCR
           03  FILLER     PIC X(8) VALUE "ENNS052 ".
      *  6. LTI-INS-TOT-ADDON
           03  FILLER     PIC X(8) VALUE "ENNS052 ".
      *  7. LTI-INS-TOT-FINCHG
           03  FILLER     PIC X(8) VALUE "ENNS052 ".
      *  8. LTI-INSCOMP
           03  FILLER     PIC X(8) VALUE "ENNN020 ".
      *  9. LTI-INSFEECD(1)
           03  FILLER     PIC X(8) VALUE "E  A020 ".
      * 10. LTI-INSFEECD(1)
           03  FILLER     PIC X(8) VALUE "E  A020 ".
      * 11. LTI-ANTICTERM
           03  FILLER     PIC X(8) VALUE "ENNN030 ".
      * 12. LTI-ORIG-1STPYDATE
           03  FILLER     PIC X(8) VALUE "ERNM060 ".
      * 13. LTI-INT-REBATE
           03  FILLER     PIC X(8) VALUE "E  A010 ".
      * 14. LTI-ANTICERN(1)
           03  FILLER     PIC X(8) VALUE "ENNS062 ".
      * 15. LTI-INT-LAST-EARNED
           03  FILLER     PIC X(8) VALUE "ENNS052N".
      * 16. LTI-CANCEL-REASON-CD.
           03  FILLER     PIC X(8) VALUE "E  A030 ".
       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 16 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

       01  LEGEND-INFO.
           03  LEGEND-A-01.
               05  FILLER           PIC X(35)  VALUE
           "F1-CANCEL, ENTER INSURANCE TYPE 1-C".
               05  FILLER           PIC X(35)  VALUE
           "L, 2-AH, 3-PP, 4-O1, 5-O2 OR 6-O3:".
           03  LEGEND-A-02.
               05  FILLER           PIC X(35)  VALUE
           "F1-CANCEL, ENTER REPAYMENT CODE RT,".
               05  FILLER           PIC X(35)  VALUE
           " ET, NP, LP OR ST:".
           03  LEGEND-A-03.
               05  FILLER           PIC X(35)  VALUE
           "F1-CANCEL, ENTER THE FIRST PAYMENT ".
               05  FILLER           PIC X(35)  VALUE
           "DATE OF THE PAYMENT ADJUSTMENT:".
           03  LEGEND-A-04.
               05  FILLER           PIC X(35)  VALUE
           "F1-CANCEL, ENTER THE TOTAL NUMBER O".
               05  FILLER           PIC X(35)  VALUE
           "F DEFERMENTS PRIOR TO THE ADDON:".
           03  LEGEND-A-05.
               05  FILLER           PIC X(35)  VALUE
           "F1-CANCEL, ENTER THE REGULAR PAYMEN".
               05  FILLER           PIC X(35)  VALUE
           "T CHANGE (+/-), IF (RT):".
           03  LEGEND-A-06.
               05  FILLER           PIC X(35)  VALUE
           "F1-CANCEL, ENTER THE TOTAL CHANGE I".
               05  FILLER           PIC X(35)  VALUE
           "N LOAN RECEIVABLE (+/-):".
           03  LEGEND-A-07.
               05  FILLER           PIC X(35)  VALUE
           "F1-CANCEL, ENTER THE TOTAL CHANGE I".
               05  FILLER           PIC X(35)  VALUE
           "N FINANCE CHARGE (+/-):".
           03  LEGEND-A-08.
               05  FILLER           PIC X(35)  VALUE
           "F1-CANCEL, IF THIS IS AN INS. ADDON".
               05  FILLER           PIC X(35)  VALUE
           ", ENTER A VALID INS. COMPANY NO:".
           03  LEGEND-A-09.
               05  FILLER           PIC X(35)  VALUE
           "F1-CANCEL, ENTER A VALID FEE CODE, ".
               05  FILLER           PIC X(35)  VALUE
           "OR (RTN):".
           03  LEGEND-A-10.
               05  FILLER           PIC X(35)  VALUE
           "F1-CANCEL, ENTER A VALID FEE CODE, ".
               05  FILLER           PIC X(35)  VALUE
           "OR (RTN):".
           03  LEGEND-A-11.
               05  FILLER           PIC X(35)  VALUE
           "F1-CANCEL, ENTER THE TERM TO EARN T".
               05  FILLER           PIC X(35)  VALUE
           "HE FINANCE CHARGE:".
           03  LEGEND-A-12.
               05  FILLER           PIC X(35)  VALUE
           "F1-CANCEL, ENTER THE EFFECTIVE DATE".
               05  FILLER           PIC X(35)  VALUE
           " OF THE ADDON:".
           03  LEGEND-A-13.
               05  FILLER           PIC X(35)  VALUE
           "F1-CANCEL, ENTER A 'Y' IF THE INTER".
               05  FILLER           PIC X(35)  VALUE
           "EST WAS REFUNDED:".
           03  LEGEND-A-14.
               05  FILLER           PIC X(35)  VALUE
           "F1-CANCEL, ENTER THE ANTICIPATED EA".
               05  FILLER           PIC X(35)  VALUE
           "RNINGS:".
       01  LEGEND-TABLE             REDEFINES LEGEND-INFO.
           03  LEGEND-TAB           OCCURS 14 TIMES
                                    PIC X(70).

      ***********************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBWI/ADONSCW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       01  ADONMN-WINDOW-HANDLE     PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/ADONMN_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/ADONMN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/ADONMNW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/ADONMN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME ADONMN-BUF EXIT-PATHNAME.

      ***********************************
       MAIN-PROGRAM SECTION.
      ***********************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE " " TO VDU-WIMENU-SELECTION.
           MOVE "ADONMN" TO VDU-FORM-NAME.

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE ADONMN-BEGIN-X * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  ADONMN-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
               COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    ADONMN-COLUMNS + VDU-WIN-W-MOD
                      LINES   ADONMN-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS ADONMN-WINDOW-HANDLE.

           MOVE ADONMN-ACCTNO TO LN-ACCTNO.
           IF ADONMN-NEW-FLAG NOT = 0
              MOVE ADONMN-PLCD           TO LN-PLCD
              MOVE ADONMN-SERVACCT       TO LN-SERVACCT
              MOVE ADONMN-SALEFIN        TO LN-SALEFIN
              MOVE ADONMN-LOANTYPE       TO LN-LOANTYPE
              MOVE ADONMN-LOANDATE       TO LN-LOANDATE
              MOVE ADONMN-ORIG-1STPYDATE TO LN-ORIG-1STPYDATE
           ELSE
              PERFORM OPEN-LN1-FILE
              PERFORM READ-LN1-FILE
              PERFORM CLOSE-LN1-FILE
              IF IO-FG NOT = 0
                 MOVE "LOAN RECORD NOT FOUND" TO MESS
                 PERFORM SEND-MESS
                 GO TO IO-ERROR.

           IF LN-LOANTYPE = "I"
              MOVE "N" TO IO-TYPE
           ELSE
              MOVE " " TO IO-TYPE.
           MOVE IO-TYPE TO CHFG(11) CHFG(12) CHFG(13) CHFG(14).
           MOVE "N" TO CHFG(16).

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.
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

      ************************************
       REC-LT-MODULE SECTION.
      ************************************
       REC-LT-MODULE-NO.

           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO NEW-FLAG FIRST-SCAN.

           MOVE LN-OWNBR      TO HOLD-LTI-BRNO   LTI-BRNO.
           MOVE ADONMN-ACCTNO TO HOLD-LTI-ACCTNO LTI-ACCTNO.

       REC-LT-INSEQ.
           MOVE
             " F7-EXIT, F5, F6-SCAN ADDON RECORDS, ENTER ADDON SEQ#:"
                                                  TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "EN N030INSEQ." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-LT-MODULE-NO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-LT-MODULE.
           IF VDUSTATUS = "1:"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-LT-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ADONSC-SCAN
              IF IO-FG = 0
                 MOVE "00" TO VDUSTATUS
              ELSE
                 GO TO REC-LT-INSEQ.
           IF VDUSTATUS NOT = "00"
              GO TO REC-LT-INSEQ.

           MOVE VDUNUM0 TO LTI-SEQNO HOLD-LTI-SEQNO.
           IF LTI-SEQNO = 0
              GO TO REC-LT-INSEQ.

       READ-LTFILE.
           PERFORM OPEN-LTI1-FILE.
           PERFORM READ-LTI1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           IF IO-FG = 0
              PERFORM DISPLAY-FIELDS
           ELSE
              IF ADONMN-TEST-BAL = 9
                 MOVE "ACCOUNT HAS BALANCES, NEW RECORDS NOT ALLOWED"
                                                       TO MESS
                 PERFORM SEND-MESS
                 GO TO REC-LT-INSEQ
              ELSE
                 PERFORM NEW-RECORD-SETUP.

           MOVE LTI-REC TO ORIGINAL-LTI-REC.

       END-REC-LT-MODULE.
           EXIT.

      *********************************************
       ADONSC-SCAN SECTION.
      *********************************************
           MOVE LN-OWNBR  TO ADONSC-BRNO.
           MOVE LN-ACCTNO TO ADONSC-ACCTNO.
           MOVE 0         TO ADONSC-KEY.
           MOVE "WI/ADONSC" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ADONSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ADONSC-STATUS NOT = "00" OR ADONSC-KEY = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-ADONSC-SCAN.

           MOVE "S  N030INSEQ." TO VDU.
           MOVE ADONSC-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE ADONSC-KEY TO VDUNUM0.

       EXIT-ADONSC-SCAN.
           EXIT.

      **********************************************
       ENTER-SCANKEY SECTION.
      **********************************************
           MOVE " F1, ENTER SEQ # OR F6 TO SCAN:" TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-LTI1-FILE.
           MOVE LN-OWNBR  TO LTI-BRNO.
           MOVE LN-ACCTNO TO LTI-ACCTNO.
           MOVE 0         TO LTI-SEQNO.


           PERFORM START-LTI1-FILE.
       ENTER-KEY.
           MOVE "EN N030SCANKEY." TO VDU.
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

           MOVE LN-OWNBR  TO LTI-BRNO.
           MOVE LN-ACCTNO TO LTI-ACCTNO.
           MOVE VDUNUM0   TO LTI-SEQNO.

           PERFORM START-LTI1-FILE.
       READ-FILES-NEXT.
           PERFORM READ-LTI1-FILE-NEXT.
           IF IO-FG = 9 
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           IF (LTI-ACCTNO NOT = LN-ACCTNO) OR
                (LTI-BRNO NOT = LTI-PATH-OWNBR)
              GO TO ENTER-KEY.

           PERFORM DISPLAY-SCAN-KEY.

           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.

       DISPLAY-SCAN-KEY.
           MOVE "SN N030INSEQ." TO VDU.
           MOVE LTI-SEQNO TO VDUNUM0.
           PERFORM SCREENIO.
       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.

           PERFORM CLOSE-LTI1-FILE.

           MOVE SPACES TO TMMSG-MESS.
           PERFORM CALL-TMMSG.
       END-SCANKEY.
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
           MOVE " F1-CLEAR, 'DELETE', ENTER FIELD#, RTN-UPDATE"
                                                     TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.

           IF ACTION = SPACES
              PERFORM TEST-AMOUNTS
              IF IO-FG = 0
                 MOVE "RWR" TO IO-TYPE
                 GO TO EXIT-ENTRY-MODULE
              ELSE
                 GO TO MODIFY-ROUTINE.

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
           IF ELE = 1
              GO TO MODIFY-ROUTINE.
       ENTER-ELE.
           IF ADONMN-TEST-BAL = 9
              MOVE "ACCOUNT HAS BALANCES, NO CHANGES ALLOWED" TO MESS
              PERFORM SEND-MESS
              GO TO MODIFY-ROUTINE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                IF CHKEY NOT = CHAN
                   GO TO MODIFY-ROUTINE.

           MOVE LEGEND-TAB(ELE) TO LEGEND.
           PERFORM SEND-LEGEND.
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
                 A-15 A-16
                 DEPENDING ON ELE.
           GO TO EXIT-ENTRY-MODULE.


      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           MOVE VDUNUM0 TO LTI-INS-TYPE.
           IF LTI-INS-TYPE > 6
              MOVE "MUST BE 1, 2, 3, 4, 5, 6 OR 0-FEE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUBUF TO LTI-TRCD.
           IF LTI-TRCD = " "
              MOVE "RT" TO LTI-TRCD.
           IF NOT (LTI-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST")
              MOVE "MUST BE RT, ET, NP, LP, OR ST" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF LTI-TRCD = "RT" OR "ST"
              MOVE " " TO CHFG(5)
           ELSE
              MOVE "N" TO CHFG(5).
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDU-DATE-YYYYMMDD TO LTI-ORIG-1STPYDATE.
           IF LTI-ORIG-1STPYDATE < LN-ORIG-1STPYDATE
              MOVE "CAN'T BE PRIOR TO LOAN FIRST PAY DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUNUM0 TO LTI-PRIOR-NODEF.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDUNUM2 TO LTI-INS-PAYMNT-INCR.
           IF VDUNUM2 = 0
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUNUM2 TO LTI-INS-TOT-ADDON.
           IF VDUNUM2 = 0
              GO TO ENTER-ELE.
           IF LTI-INS-PAYMNT-INCR NOT = 0
              IF LTI-INS-TOT-ADDON < 0
                 IF LTI-INS-PAYMNT-INCR NOT < 0
                    GO TO ENTER-ELE
                 ELSE
                    NEXT SENTENCE
              ELSE
                 IF LTI-INS-TOT-ADDON > 0
                    IF LTI-INS-PAYMNT-INCR NOT > 0
                       GO TO ENTER-ELE.

       A-07.
           MOVE VDUNUM2 TO LTI-ANTICERN(3).
              IF LTI-INS-TOT-ADDON < 0
                 IF LTI-ANTICERN(3) NOT < 0
                    GO TO ENTER-ELE
                 ELSE
                    NEXT SENTENCE
              ELSE
                 IF LTI-ANTICERN(3) < 0
                    GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-08.
           IF LTI-INS-TYPE = 0
              IF VDUNUM0 NOT = 0
                 MOVE
                 "0 TYPE = FEE ADDON, INSURANCE ENTRY NOT ALLOWED"
                                                       TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF LTI-INS-TYPE NOT = 0
              IF VDUNUM0 = 0
                 MOVE "INSURANCE ENTRY REQUIRED!" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           MOVE 1 TO I-LEVEL.
           PERFORM INSURANCE-ENTRY.
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
           PERFORM FEE-ENTRY.
           IF IO-FG = 8
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDUNUM0 TO LTI-ANTICTERM.
           IF VDUNUM0 = 0
              IF LN-LOANTYPE = "P"
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       A-12.
           MOVE VDU-DATE-YYYYMMDD TO LTI-ANTIC-START-DATE.
           IF VDUNUM0 = 0
              IF LN-LOANTYPE = "P"
                 GO TO ENTER-ELE.

           IF LTI-ANTIC-START-DATE < LN-LOANDATE
              MOVE "CAN'T BE PRIOR TO LOAN DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       A-13.
           IF NOT (VDUBUF = " " OR "Y" OR "C" OR "R")
              MOVE "ENTER: ' ', 'Y', 'C' OR 'R'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LTI-INT-REBATE.
           GO TO CREATE-ROUTINE.

       A-14.
           MOVE VDUNUM2 TO LTI-ANTICERN(ELE - 13).
           IF LTI-ANTICERN(3) = LTI-ANTICERN(ELE - 13)
                 GO TO A-141
              ELSE
                 GO TO ENTER-ELE.

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
           MOVE "SNNN062  5." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE 0 TO LTI-UFERN(ELE - 13).
           GO TO CREATE-ROUTINE.

       A-15.
           MOVE VDUNUM2 TO LTI-INT-LAST-EARNED.
           GO TO CREATE-ROUTINE.
       A-16.
           MOVE VDUBUF       TO LTI-CANCEL-REASON-CD.
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
              MOVE SPACES TO LTI-INSOURS
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
              MOVE " " TO LTI-CANCEL-REASON-CD
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
           IF VDUBUF = " "
              MOVE "S  A010  1." TO VDU
              MOVE ELE TO VDUROW
              MOVE "Y" TO VDUBUF
              PERFORM SCREENIO.
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
           MOVE VDU-DATE-YYYYMMDD TO LTI-INSEFF-DATE.
           IF LTI-INSEFF-DATE = 0
              GO TO ENTER-IC-EFF.

           IF LTI-INSEFF-DATE < LN-LOANDATE
              MOVE "CAN'T BE PRIOR TO LOAN DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

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
           MOVE VDU-DATE-YYYYMMDD TO LTI-INSEXP-DATE.
           IF LTI-INSEXP-DATE = 0
              GO TO ENTER-IC-EXP.

           IF LTI-INSEXP-DATE < LTI-INSEFF-DATE
              MOVE "CAN'T BE PRIOR TO INS. EFFECTIVE DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

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
           IF LTI-INSCOVR = 0
              GO TO ENTER-IC-COVR.

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
           IF LTI-INS-TOT-ADDON < 0
              IF LTI-INSPREM NOT < 0
                 GO TO ENTER-IC-PREM
              ELSE
                 NEXT SENTENCE
           ELSE
              IF LTI-INS-TOT-ADDON > 0
                 IF LTI-INSPREM NOT > 0
                    GO TO ENTER-IC-PREM.

           MOVE ADONMN-BRANCH TO BR-NO.
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
              GO TO EXIT-ENTER-INSURANCE.

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
           IF LTI-INSPREM NOT < 0
              IF LTI-INSCOMM > LTI-INSPREM
                 GO TO ENTER-IC-COMM
              ELSE
                 NEXT SENTENCE
           ELSE
              IF LTI-INSCOMM < LTI-INSPREM
                 GO TO ENTER-IC-COMM.

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
              MOVE 0 TO LTI-INSFEE(ELE - 8)
              MOVE SPACES TO LTI-INSFEECD(ELE - 8)
              GO TO EXIT-FEE-ENTRY.

           MOVE VDUBUF TO LTI-INSFEECD(ELE - 8) CDB-CODE.
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

           MOVE VDUNUM2 TO LTI-INSFEE(ELE - 8).

       EXIT-FEE-ENTRY.
           EXIT.

      *****************************************
       TEST-AMOUNTS SECTION.
      *****************************************
           MOVE 0 TO IO-FG.
           IF LTI-INS-TOT-ADDON < 0
              MOVE -1 TO ONE
           ELSE
              MOVE 1 TO ONE.

           COMPUTE WORK-AMT =
              LTI-INSPREM + LTI-INSFEE(1) * ONE + LTI-INSFEE(2) * ONE.

           IF LN-LOANTYPE = "P"
              ADD LTI-ANTICERN(1) TO WORK-AMT.

           IF WORK-AMT NOT = LTI-INS-TOT-ADDON
              MOVE "INCREASE IN LOAN AMOUNT DOES NOT BALANCE" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
              GO TO TEST-AMOUNTS-EXIT.

           IF LN-LOANTYPE = "P"
              IF LTI-ANTICERN(1) NOT = LTI-ANTICERN(3)
                 MOVE "FINANCE CHARGES DO NOT MATCH" TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
                 GO TO TEST-AMOUNTS-EXIT.

       TEST-AMOUNTS-EXIT.
           EXIT.

      **********************************
       WRITE-MODULE SECTION.
      **********************************
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

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-LTI1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-LTI1-FILE.

      *****************************
       DELETE-MODULE SECTION.
      *****************************
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

      *******************************
       DISPLAY-FIELDS SECTION.
      *******************************
           IF LTI-TRCD = "RT" OR "ST"
              MOVE " " TO CHFG(5)
           ELSE
              MOVE "N" TO CHFG(5).
           MOVE LTI-INS-TYPE TO F01.
           MOVE LTI-TRCD     TO F02.
           MOVE LTI-ORIG-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F03.
           MOVE LTI-PRIOR-NODEF TO F04.
           MOVE LTI-INS-PAYMNT-INCR TO F05.
           MOVE LTI-INS-TOT-ADDON TO F06.
           MOVE LTI-ANTICERN(3) TO F07.
           MOVE LTI-INSCOMP TO F08.
           MOVE LTI-INSOURS TO F081.
           MOVE LTI-INSEFF-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO F082.
 
           MOVE LTI-INSEXP-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO F083.
           MOVE 1 TO ITRM-SUB.
           PERFORM MISC-INS-TERM.
           MOVE ITRM-INSTERM  TO F084.
           MOVE LTI-INSCOVR TO F085.
           MOVE LTI-INSPREM TO F086.
           MOVE LTI-INSCOMM TO F087.
           MOVE LTI-INS-REBATE TO F088.

           MOVE LTI-INSFEECD(1) TO F09 CDB-CODE.
           PERFORM GET-CODE.
           MOVE CDB-DESC    TO F091.
           MOVE LTI-INSFEE(1) TO F092.

           MOVE LTI-INSFEECD(2) TO F10 CDB-CODE.
           PERFORM GET-CODE.
           MOVE CDB-DESC    TO F101.
           MOVE LTI-INSFEE(2) TO F102.

           MOVE 0 TO SUB.
           PERFORM DISPLAY-FIELDS-COMPONENTS 1 TIMES.

           MOVE LTI-ANTICTERM TO F11.
           MOVE LTI-ANTIC-START-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F12.
 
           MOVE LTI-INT-REBATE TO F13.
           MOVE LTI-INT-LAST-EARNED TO F15.
           MOVE LTI-CANCEL-REASON-CD TO F16.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************
       DISPLAY-FIELDS-COMPONENTS SECTION.
      ******************************************
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
              MOVE "  REBATED?" TO F4-ALP(1).

       DISP-FIELDS-EXIT.
           EXIT.

      ***************************************
       NEW-RECORD-SETUP SECTION.
      ***************************************
           MOVE 1 TO NEW-FLAG.

           MOVE "S  A000DISP." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-LTI-FILE.
           MOVE HOLD-LTI1-KEY TO LTI1-KEY.

           MOVE 0 TO ELE.

           MOVE " " TO CHFG(5).

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPLTICL.CPY".

      ***********************************************
       FORM-RESET SECTION.
      ***********************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY ADONMN-SCREEN UPON ADONMN-WINDOW-HANDLE.
           MOVE ADONMN-FORM-FIELDS TO WR-BUF.
           MOVE ADONMN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE ADONMN-HELP--FILE TO HELP-LINK-FILE.
           MOVE ADONMN-HELP--DIR  TO HELP-LINK-DIR.

      ***********************************************
       VDU-CONVERT-FIELD SECTION.
      ***********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/ADONMN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************************
       MISC-PARAGRAPHS SECTION.
      ***************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       GET-CODE.
           MOVE "GC"          TO CDB-TYPE.
           MOVE ADONMN-BRANCH TO CDB-BRANCH.
      *--> SET BEFORE            CDB-CODE.
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

      ***************************************
       IO-ROUTINES SECTION.
      ***************************************
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
      ***************************************
       END-ROUTINE SECTION.
      ***************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ADONMN-SCREEN.
           DESTROY ADONMN-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
