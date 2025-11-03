       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BILNSC.
      ************************************************************
      *
      *              GLOBAL BORROWER / LOANS FILE WINDOW SCAN
      *
      * CALLED FROM: CL/BISCAN, CL/WIMENU, SP/SPINQ, LP/INBRTR, MN/EPMAIN
      *              SP/CPINQ
      * REV:
      *  BLL 061295 CHANGED TO NOT LOOP IF TOO MANY LOANS FOR 1 SOC
      *  MJD 092595 ADDED BILNSC-ERROR. INPUT: "T" INDICATES THAT
      *             CALL WAS MADE FROM INBRTR.C. OUTPUT: "Y" INDICATES
      *             THAT PROGRAM WAS ABORTED VIA F7.
      *  BLL 100995 CHANGED PAGE-KEY-N FROM 22 BYTEST TO 32 TO
      *             FIX LOOPING ON ALPHA SCAN; ADD BR-FILE TO DECLARE
      *  MJD 101195 CHECK FOR POFF LOANS WHILE BUILDING PAGE KEY TABLE
      *  JTG 101995 T E M P O R Y   REMOVED READ-BWFILE
      *  JTG 102295 REMOVED T E M P O R Y   REMOVED READ-BWFILE
      *  MJD 111095 ADDED LOGIC TO SCAN BY NAME IF CALLED FROM
      *              INTER BRANCH TRANSACTIONS.
      *  JFF 112295 ADDED LOGIC TO CALL BWLNS2 RE: F3
      *  JTG 060297 ADDED DISPLY OF LEGEND-04G & LEGEND-07G
      *             FOR GLOBAL LOAN SCAN
      *  MJD 061897 MOVED BILNSC-SYSDATE TO BWLNS2-SYSDATE.
      *  BAH 061897 ADDED PAYMENT AMT AND COLLATERAL TO MORE INFO, FOR COLTN
      *  BAH 102897 REMOVED TEST OF PAIDOFF LOANS NOT DISPLAYING
      *             IF BORROWER HAS OPEN LOANS. WAS WORLDS CODE, BUT
      *             THEIR PURGE SHOULD HANDLE THAT LOAD NOW.
      *  JTG 990418 ADDED READ ON SP-REC FOR RVPMTDUE ROUTINE IN SPPDTH
      *  BLV 000523 IF BAD BRANCH, BWLNS2-INFO WAS BRANCHING OUT OF A
      *             PERFORM BY GOING TO MORE-INFO-EXIT, CONFUSED PROGRAM &
      *             PUT USER INTO A LOOP OF "MORE INFO NOT AVAILABLE"
      *  BLV 010518 EXPANDED PAGE KEY TABLE FROM 50 TO 100
      *   CS 141114 CHANGED EXT-WCOLOR/EXT-WCTRL FROM TCLP TO SCAN
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *  BLM 160914 WAS NOT RETURNING ACCT# CORRECTLY, FIXED TO 8 DIGIT & GOT
      *             RID OF SEQNO
      *  BLM 161221 FIX ALPHA SCAN FOR LONGER BORROWER NAMES
      *  BAH 170323 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 171117 FIX PAGE-KEY AND BI1-KEY
      *  BAH 181218 GLOBAL LNFILE
      *  BAH 20191007 REMOVED ACCESS-CALL LNFILE, BWFILE
      *  BAH 20240122 ADDED BILNSC-CALLED-FROM TO SPEED UP FINDING 1 SSNO
      *  MB  20240422 CHANGED TO USE SSP TO RETRIEVE BIFILE DATA.
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

      *********************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/BILNSC.CBL S35 02/22/24-08:41:55 >".
       COPY "LIBLP/LP01BI.CPY".
       COPY "LIBLP/LP01BI_SQL.CPY".
       COPY "LIBLP/LPWSBI.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LH.CPY".
       COPY "LIBLP/LP01LH_SQL.CPY".
       COPY "LIBLP/LPWSLH.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-SEL-TYPE               PIC X.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  BI1-CONN-OPEN-SW            PIC X       VALUE "N".

       01  BIDATA-ARRAY-CURRENT-ROW    PIC 9(3)    VALUE 0.
       01  BIDATA-ARRAY-CTR            PIC 9(3)    VALUE 0.

       01  BIDATA-ARRAY.
          03  BIDATA-ARRAY-DATA        OCCURS 900 TIMES
                                       PIC X(BI-SIZE).
      

       01  BILNSC-LINES            PIC 99  VALUE 17.
       01  BILNSC-COLUMNS          PIC 99  VALUE 78.
       01  BILNSC-BEGIN-Y          PIC 99  VALUE 5.
       01  BILNSC-BEGIN-X          PIC 99  VALUE 2.
       01  ERRCD                    PIC X    VALUE SPACES.
       01  HOLD-BR-KEY              PIC 9(4) VALUE 0.
       01  HOLD-LN1-KEY             PIC 9(12).

       01  ONE                      PIC S9     VALUE 1.
       01  IX1                      PIC 99.
       01  IX2                      PIC 99.
       01  SUB                      PIC 99.
       01  SCAN-TYPE                PIC 9      VALUE 2.

       01  ALP-BI.
           03  ALP-BI123            PIC X(3).
           03  ALP-HYPHEN1          PIC X.
           03  ALP-BI45             PIC X(2).
           03  ALP-HYPHEN2          PIC X.
           03  ALP-BI6789           PIC X(4).

       01  NUM-BI                   PIC 9(9).
       01  FILLER                   REDEFINES NUM-BI.
           03  NUM-BI123            PIC X(3).
           03  NUM-BI45             PIC X(2).
           03  NUM-BI6789           PIC X(4).

       01  A-NAME                   PIC X(35).

       01  BI-ALP-TABLE.
           03  BI-ALP-TBL           OCCURS 70 TIMES
                                    PIC X.

       01  FLDSEL-STAT              PIC XX.
       01  FLDSEL-FIELD             PIC X(4)   VALUE "010.".
       01  FLDSEL-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           03  FLDSEL-S.
               05  FLDSEL-S-SSNO      PIC X(11).
               05  FILLER             PIC X(25).
               05  FLDSEL-S-ACCTNO    PIC 9(8).
               05  FILLER             PIC X(5).
               05  FLDSEL-S-OWNBR     PIC 9(4).
               05  FILLER             PIC X(23).
           03  FLDSEL-N               REDEFINES FLDSEL-S.
               05  FLDSEL-N-NAME      PIC X(20).
               05  FILLER             PIC X.
               05  FLDSEL-N-SSNO      PIC X(11).
               05  FILLER             PIC X(4).
               05  FLDSEL-N-ACCTNO    PIC 9(8).
               05  FILLER             PIC X(5).
               05  FLDSEL-N-OWNBR     PIC 9(4).
               05  FILLER             PIC X(23).

       01  FIRST-FIELD              PIC X(4)  VALUE "010.".
       01  LAST-FIELD               PIC X(4)  VALUE "090.".

       01  PROMPT-LIST              PIC X(6)  VALUE "000:S.".

       01  DISPLAY-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090 ".
           03  FILLER               PIC X(15)  VALUE
           "PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE               OCCURS 9 TIMES.
               05  D-S.
                   07  D-S-KEY      PIC X(11).
                   07  FILLER       PIC XX.
                   07  D-S-NAME     PIC X(20).
                   07  FILLER       PIC XX.
                   07  D-S-ACCTNO   PIC Z(9).
                   07  FILLER       PIC XX.
                   07  D-S-MAKERCD  PIC X.
                   07  FILLER       PIC XX.
                   07  D-S-OWNBR    PIC Z(4).
                   07  FILLER       PIC XX.
                   07  D-S-ORGST    PIC XX.
                   07  FILLER       PIC XX.
                   07  D-S-ENTDATE  PIC 99/99/99   BLANK ZERO.
                   07  FILLER       PIC X.
                   07  D-S-POFFDATE PIC 99/99/99   BLANK ZERO.
               05  D-N              REDEFINES D-S.
                   07  D-N-NAME     PIC X(20).
                   07  FILLER       PIC X.
                   07  D-N-KEY      PIC X(11).
                   07  FILLER       PIC XXX.
                   07  D-N-ACCTNO   PIC Z(9).
                   07  FILLER       PIC XX.
                   07  D-N-MAKERCD  PIC X.
                   07  FILLER       PIC XX.
                   07  D-N-OWNBR    PIC Z(4).
                   07  FILLER       PIC XX.
                   07  D-N-ORGST    PIC XX.
                   07  FILLER       PIC XX.
                   07  D-N-ENTDATE  PIC 99/99/99   BLANK ZERO.
                   07  FILLER       PIC X.
                   07  D-N-POFFDATE PIC 99/99/99   BLANK ZERO.
           03  D-PAGENO             PIC ZZ9.
           03  D-END-MORE           PIC X(20).

       01  CURRENT-PAGE             PIC S999.
       01  PAGE-COUNT               PIC 999.
       01  MAX-PAGES                PIC 999    VALUE 100.
       01  PAGE-KEY.
           03  FILLER                 OCCURS 100 TIMES.
               05  PAGE-KEY-N-X.
                   07  PAGE-KEY-N     PIC X(70).
                   07  PAGE-KEY-BI1-KEY.
                       09  PAGE-KEY-SSNO   PIC 9(9) COMP.
                       09  PAGE-KEY-ACCTNO PIC 9(8) COMP.
                       09  PAGE-KEY-OWNBR  PIC 9(4) COMP.


       01  WORK-KEY-BI1-KEY.
           03  WORK-KEY-SSNO   PIC 9(9).
           03  WORK-KEY-ACCTNO PIC 9(8).
           03  WORK-KEY-OWNBR  PIC 9(4).
       01  TITLE-LIST          PIC X(6)  VALUE "TITLE.".
       01  MORE-LIST.
           03  FILLER               PIC X(39)  VALUE
           "TITLE ADR1 ADR2 ADR3 PHONE EMPL SPOUSE ".
           03  FILLER               PIC X(32)  VALUE
           "LIA OPEN CLOSED PAY PAYS COLLID.".
       01  MORE-BUF.
           03  M-NAME               PIC X(30).
           03  M-ADR1               PIC X(30).
           03  M-ADR2               PIC X(30).
           03  M-ADR3               PIC X(30).
           03  M-PHONE              PIC X(14).
           03  M-EMPL               PIC X(20).
           03  M-SPOUSE             PIC X(28).
           03  M-LIA                PIC Z,ZZZ,ZZ9.99-.
           03  M-OPEN               PIC ZZ.
           03  M-CLOSED             PIC ZZ.
           03  M-PAY                PIC Z(5).99.
           03  M-PAY-S              PIC X.
           03  M-COLLID             PIC X(35).

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBWI/BWLNS2W.CPY".

       COPY "LIBWI/BILNSC_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/FLDSELW.CPY".

       01  BILNSC-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/BILNSC_DEF.CPY".
       COPY "LIBWI/BISCN2_DEF.CPY".
       COPY "LIBWI/BWLNS2_DEF.CPY".
       COPY "LIBWI/BILNSC_WKS.CPY".
       COPY "LIBWI/BISCN2_WKS.CPY".
       COPY "LIBWI/BWLNS2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/BILNSCW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/BILNSC_SCN.CPY".
       COPY "LIBWI/BISCN2_SCN.CPY".
       COPY "LIBWI/BWLNS2_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME BILNSC-BUF EXIT-PATHNAME.

      ****************************
       MAIN-PROGRAM SECTION.
      ****************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "BILNSC" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE BILNSC-BEGIN-Y * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  BILNSC-BEGIN-X * VDU-WIN-C-OFFSET
                   SIZE    BILNSC-COLUMNS + VDU-WIN-W-MOD
                   LINES   BILNSC-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS BILNSC-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           IF BILNSC-ERROR = "T"
              MOVE TITLE-02 TO WR-BUF
              MOVE TITLE-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM GET-GPENV.

           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-BI-RECORD.

           IF BILNSC-SSNO NOT = 0
              MOVE 1 TO SCAN-TYPE
              PERFORM SEND-SORT-PROMPT
              MOVE BILNSC-SSNO TO VDUBUF
              GO TO ENTER-SCANKEY-CONT.

           IF BILNSC-ERROR = "G" OR "T"
              MOVE 2 TO SCAN-TYPE
           ELSE
              MOVE 1 TO SCAN-TYPE.
           PERFORM SEND-SORT-PROMPT.
           GO TO ENTER-SCANKEY.

      *************************************
      *    ENTER BORROWER SCAN TYPE
      *************************************
       ENTER-TYPE.
           MOVE "S  A050ATYPE:S." TO VDU.
           MOVE LEGEND-06 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010TYPE." TO VDU.
           MOVE "_" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE LEGEND-01 TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN010TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "Y" TO BILNSC-ERROR
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TYPE.
           IF NOT (VDUNUM0 = 1 OR 2)
              GO TO ENTER-TYPE.
           MOVE VDUNUM0 TO SCAN-TYPE.

           PERFORM SEND-SORT-PROMPT.

      *************************************
      *    ENTER SCAN KEY
      *************************************
       ENTER-SCANKEY.
           MOVE "S  A050ASCAN:S." TO VDU.
           MOVE LEGEND-05 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A240SORTKEY." TO VDU.
           MOVE "_______________________" TO VDUBUF.
           PERFORM SCREENIO.

           IF SCAN-TYPE = 1
              MOVE LEGEND-02 TO MESS
           ELSE
              MOVE LEGEND-03 TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A200SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-TYPE.
           IF VDUSTATUS = "1>"
              MOVE "Y" TO BILNSC-ERROR
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SCANKEY.

       ENTER-SCANKEY-CONT.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           IF SCAN-TYPE = 1
              INSPECT VDUBUF REPLACING ALL " " BY "0"
              MOVE VDUBUF TO WORK-KEY-BI1-KEY
              IF WORK-KEY-BI1-KEY NOT NUMERIC
                 GO TO ENTER-SCANKEY
              ELSE
                 MOVE WORK-KEY-SSNO TO PAGE-KEY-SSNO(PAGE-COUNT)
                 MOVE WORK-KEY-ACCTNO TO PAGE-KEY-ACCTNO(PAGE-COUNT)
                 MOVE WORK-KEY-OWNBR TO PAGE-KEY-OWNBR(PAGE-COUNT).
           IF SCAN-TYPE = 2
              PERFORM ARRANGE-BI-KEY
              MOVE BI-ALP-TABLE TO PAGE-KEY-N(PAGE-COUNT)
              MOVE 0 TO PAGE-KEY-SSNO(PAGE-COUNT)
                        PAGE-KEY-ACCTNO(PAGE-COUNT)
                        PAGE-KEY-OWNBR(PAGE-COUNT).

           IF SCAN-TYPE = 1
              MOVE PAGE-KEY-SSNO(1)   TO BI-SSNO
              MOVE PAGE-KEY-ACCTNO(1) TO BI-ACCTNO
              MOVE PAGE-KEY-OWNBR(1)  TO BI-OWNBR
           ELSE
              MOVE PAGE-KEY-N(1) TO BI2-KEY.

           MOVE SPACES TO DISPLAY-BUF.
           MOVE "* RETRIEVING DATA *" TO D-END-MORE.
           MOVE DISPLAY-BUF  TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           PERFORM LOAD-BIDATA-ARRAY.

       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.

           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.
           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

      *     PERFORM OPEN-BI-RECORD.

           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1 TO SUB.
      *     IF SCAN-TYPE = 1
      *        MOVE PAGE-KEY-SSNO(CURRENT-PAGE)   TO BI-SSNO
      *        MOVE PAGE-KEY-ACCTNO(CURRENT-PAGE) TO BI-ACCTNO
      *        MOVE PAGE-KEY-OWNBR(CURRENT-PAGE)  TO BI-OWNBR
      *     ELSE
      *        MOVE PAGE-KEY-N(CURRENT-PAGE) TO BI2-KEY.

       BUILD-PAGENO.
      *     PERFORM READ-BI-RECORD.
      *     IF IO-FG NOT = 0
      *        GO TO DISPLAY-PAGE.

           IF BIDATA-ARRAY-CURRENT-ROW + 1 > BIDATA-ARRAY-CTR
              GO TO DISPLAY-PAGE.

           IF SUB > 9
              GO TO DISPLAY-PAGE.

           ADD 1 TO BIDATA-ARRAY-CURRENT-ROW.
           MOVE BIDATA-ARRAY-DATA(BIDATA-ARRAY-CURRENT-ROW) TO BI-REC.

           IF BILNSC-SSNO NOT = 0
              IF BILNSC-SSNO NOT = BI-SSNO
                 MOVE 9 TO IO-FG
                 GO TO DISPLAY-PAGE.

           MOVE BI-SSNO TO NUM-BI.
           IF NUM-BI = 0
              GO TO BUILD-PAGENO.
           PERFORM NUMBI-TO-ALPBI.
           IF SCAN-TYPE = 1
              MOVE ALP-BI TO D-S-KEY(SUB)
           ELSE
              MOVE ALP-BI TO D-N-KEY(SUB).

           MOVE BI-LNAME TO MESS.
           MOVE BI-FNAME TO A-NAME.
           PERFORM SET-BI-LNAME.
           IF SCAN-TYPE = 1
              MOVE MESS        TO D-S-NAME(SUB)
              MOVE BI-ACCTNO   TO D-S-ACCTNO(SUB)
              MOVE BI-OWNBR    TO D-S-OWNBR(SUB)
              MOVE BI-MAKERCD  TO D-S-MAKERCD(SUB)
              MOVE BI-ORGST    TO D-S-ORGST(SUB)
              MOVE BI-POFFDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-S-POFFDATE(SUB)
              MOVE BI-ENTDATE  TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-S-ENTDATE(SUB)
           ELSE
              MOVE MESS        TO D-N-NAME(SUB)
              MOVE BI-ACCTNO   TO D-N-ACCTNO(SUB)
              MOVE BI-OWNBR    TO D-N-OWNBR(SUB)
              MOVE BI-MAKERCD  TO D-N-MAKERCD(SUB)
              MOVE BI-ORGST    TO D-N-ORGST(SUB)
              MOVE BI-POFFDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-N-POFFDATE(SUB)
              MOVE BI-ENTDATE  TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-N-ENTDATE(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      *************************************
      *    DISPLAY PAGE
      *************************************
       DISPLAY-PAGE.

      *     IF IO-FG NOT = 0
           IF BIDATA-ARRAY-CTR = 0  
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-MESS
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 IF BILNSC-SSNO = 0
                    GO TO ENTER-SCANKEY
                 ELSE
                    GO TO END-ROUTINE.

           MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
      *        IF IO-FG = 0
              IF BIDATA-ARRAY-CURRENT-ROW < BIDATA-ARRAY-CTR  
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    IF SCAN-TYPE = 1
                       MOVE BI-SSNO   TO PAGE-KEY-SSNO(PAGE-COUNT)
                       MOVE BI-ACCTNO TO PAGE-KEY-ACCTNO(PAGE-COUNT)
                       MOVE BI-OWNBR  TO PAGE-KEY-OWNBR(PAGE-COUNT)
                    ELSE
                       MOVE BI2-KEY   TO PAGE-KEY-N(PAGE-COUNT)
                       MOVE BI-SSNO   TO PAGE-KEY-SSNO(PAGE-COUNT)
                       MOVE BI-ACCTNO TO PAGE-KEY-ACCTNO(PAGE-COUNT)
                       MOVE BI-OWNBR  TO PAGE-KEY-OWNBR(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.

           MOVE CURRENT-PAGE TO D-PAGENO.
           MOVE DISPLAY-BUF  TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *************************************
      *    SELECT FIELD FROM PAGES
      *************************************
       SELECT-FIELD-FROM-DISPLAY.
           IF BILNSC-SSNO = 0
              IF BILNSC-ERROR = "T"
                 MOVE LEGEND-04 TO MESS
              ELSE
                 MOVE LEGEND-04G TO MESS
           ELSE
              IF BILNSC-ERROR = "T"
                 MOVE LEGEND-07 TO MESS
              ELSE
                 MOVE LEGEND-07G TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.

      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 IF CURRENT-PAGE = 2
                    MOVE 0 TO BIDATA-ARRAY-CURRENT-ROW
                 ELSE 
                    COMPUTE BIDATA-ARRAY-CURRENT-ROW = 
                      (CURRENT-PAGE - 2) * 9
                 END-IF
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"    *> PAGE UP
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 IF CURRENT-PAGE = 2
                    MOVE 0 TO BIDATA-ARRAY-CURRENT-ROW  
                 ELSE 
                    COMPUTE BIDATA-ARRAY-CURRENT-ROW = 
                       (CURRENT-PAGE - 2) * 9
                 END-IF
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST HOME:
           IF FLDSEL-STAT = "1H"    *> HOME KEY
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 0 TO BIDATA-ARRAY-CURRENT-ROW
              GO TO START-BUILD-PAGENO.

           IF FLDSEL-STAT = "1>"     *> F7
              MOVE "Y" TO BILNSC-ERROR
              GO TO END-ROUTINE.
 
           IF FLDSEL-STAT = "10"     *> F1
              IF BILNSC-SSNO = 0
                 GO TO ENTER-SCANKEY.

           IF FLDSEL-STAT = "16"     *> F3
              MOVE "N" TO BWLNS2-ALL-LOANS
              PERFORM BWLNS2-INFO
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF FLDSEL-STAT = "18"   *> F4
              MOVE "Y" TO BWLNS2-ALL-LOANS
              PERFORM BWLNS2-INFO
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF FLDSEL-STAT = "1<"    *> F6
              PERFORM MORE-INFO
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF FLDSEL-STAT NOT = "00"   *> ENTER
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF FLDSEL-STAT = "00" AND BILNSC-ERROR = "T"
              IF SCAN-TYPE = 1
                 MOVE FLDSEL-N-ACCTNO TO BILNSC-ACCTNO
                 MOVE FLDSEL-N-OWNBR  TO BILNSC-BRANCH
              ELSE
                 MOVE FLDSEL-S-ACCTNO TO BILNSC-ACCTNO
                 MOVE FLDSEL-S-OWNBR  TO BILNSC-BRANCH
              END-IF
              GO TO END-ROUTINE
           ELSE
              GO TO SELECT-FIELD-FROM-DISPLAY.

           GO TO END-ROUTINE.

      *************************************
       MORE-INFO SECTION.
      *************************************
           PERFORM CLOSE-BI-RECORD.

           PERFORM GET-BR-KEY.

           PERFORM GET-BR.
           IF IO-FG = 9
              PERFORM SEND-MORE-INFO
              GO TO MORE-INFO-EXIT.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTC-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE.

           PERFORM GET-BW-GLOBAL.
           IF IO-FG NOT = 0
              MOVE "UNABLE TO READ BORROWER RECORD" TO MESS
              PERFORM SEND-MESS
              GO TO MORE-INFO-EXIT.

           PERFORM GET-LN-GLOBAL.
           IF IO-FG NOT = 0
              MOVE "UNABLE TO READ LOAN RECORD" TO MESS
              PERFORM SEND-MESS
              GO TO MORE-INFO-EXIT.

           MOVE 17 TO WINDOW-LINES.
           MOVE 50 TO WINDOW-COLUMNS.
           MOVE 2 TO WINDOW-BEGIN-Y.
           MOVE 2 TO WINDOW-BEGIN-X.
           MOVE FLDSEL-FIELD TO WINDOW-FIT-TO.
           MOVE "WI/BISCN2" TO WINDOW-FORM-NAM.
           MOVE "BISCN2" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY BISCN2-SCREEN.

           MOVE BISCN2-FORM-FIELDS TO WR-BUF.
           MOVE BISCN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE BW-FNAME TO MESS.
           MOVE BW-LNAME TO A-NAME.
           PERFORM SET-BI-FNAME.
           MOVE MESS TO M-NAME.
           PERFORM SET-CITY-STATE-ZIP.
           IF BW-ADR2 = " "
              MOVE BW-ADR1 TO M-ADR1
              MOVE MESS TO M-ADR2
              MOVE " " TO M-ADR3
           ELSE
              MOVE BW-ADR1 TO M-ADR1
              MOVE BW-ADR2 TO M-ADR2
              MOVE MESS TO M-ADR3.
           MOVE BW-PHONE TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT TO M-PHONE.
           MOVE BW-EMPLOYER TO M-EMPL.
           MOVE BW-SPFNAME TO MESS.
           MOVE BW-SPLNAME TO A-NAME.
           PERFORM SET-BI-FNAME.
           MOVE MESS TO M-SPOUSE.
           ADD BW-LIASMLN BW-LIAOTH GIVING M-LIA.
           MOVE BW-LOANS TO M-OPEN.
           MOVE BW-PRLOANS TO M-CLOSED.

      * NEED TO GET CURRENT SCHEDULED PAYMENT AMT
           PERFORM GET-SP.
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-CUR-SCHD-PAY TO M-PAY.
           IF LN-PAY-SCHLD-FG = "Y"
              MOVE "S" TO M-PAY-S.

           IF LN-COLLID NOT = SPACES
              MOVE LN-COLLID TO M-COLLID
           ELSE
              PERFORM GET-LT
              IF IO-FG = 0
                 IF LTC-MAKE(1) NOT = SPACES OR LTC-VIN(1) NOT = SPACES
                    MOVE LTC-YEAR(1) TO VDU
                    INSPECT VDU REPLACING FIRST "    " BY " +++"
                    INSPECT VDU REPLACING FIRST "+++            "
                                                       BY LTC-MAKE(1)
                    INSPECT VDU REPLACING FIRST "    " BY " +++"
                    INSPECT VDU REPLACING FIRST "+++            "
                                                       BY LTC-MODEL(1)
                    INSPECT VDU REPLACING FIRST "    " BY " +++"
                    INSPECT VDU REPLACING FIRST "+++     "
                                                       BY LTC-STYLE(1)
                    INSPECT VDU REPLACING FIRST "    " BY " +++"
                    INSPECT VDU REPLACING FIRST "+++                 "
                                                       BY LTC-VIN(1)
                    MOVE VDU TO M-COLLID
                 ELSE
                    MOVE LTC-HHGS(1) TO M-COLLID
               ELSE
                 MOVE SPACES TO M-COLLID.

           MOVE MORE-BUF TO WR-BUF.
           MOVE MORE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A010CONTINUE." TO VDU.
           PERFORM SCREENIO.

       MORE-INFO-CLOSE-WINDOW.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BISCN2-SCREEN.
           PERFORM CLOSE-WINDOW.
           MOVE "BILNSC" TO VDU-FORM-NAME.
       MORE-INFO-EXIT.
           PERFORM OPEN-BI-RECORD.

      ***********************************************
       LOAD-BIDATA-ARRAY SECTION.
      ***********************************************
           INITIALIZE BIDATA-ARRAY.
           MOVE 0 TO BIDATA-ARRAY-CTR 
                     BIDATA-ARRAY-CURRENT-ROW.

           PERFORM START-BI-RECORD.  *> SSP  BIFILE

       LOAD-BIDATA-ARRAY-LOOP.

           PERFORM READ-BI1-FILE-NEXT.  *> SSP  BIFILE
           IF IO-FG NOT = 0
              PERFORM CLOSE-BI1-FILE
              GO TO LOAD-BIDATA-ARRAY-EXIT.

           ADD 1 TO BIDATA-ARRAY-CTR.
           MOVE BI-REC TO BIDATA-ARRAY-DATA(BIDATA-ARRAY-CTR).

           GO TO LOAD-BIDATA-ARRAY-LOOP.

       LOAD-BIDATA-ARRAY-EXIT.
           EXIT.

      ***********************************************
       BWLNS2-INFO SECTION.
      ***********************************************
           PERFORM CLOSE-BI-RECORD.

           PERFORM GET-BR-KEY.
           PERFORM GET-BR.
           IF IO-FG = 9
              PERFORM SEND-MORE-INFO
              GO TO BWLNS2-INFO-EXIT.

           MOVE "Y"            TO BWLNS2-SCAN-ONLY.
           MOVE NUM-BI         TO BWLNS2-KEY.
      * BILNSC-SYSDATE X(8) MM/DD/YY, SO IS BWLNS2
           MOVE BILNSC-SYSDATE TO BWLNS2-SYSDATE.
           MOVE BR-MACHINE     TO BWLNS2-BR-MACHINE.
           MOVE BR-DATA-PATH   TO BWLNS2-BR-DATA-PATH.
           MOVE "WI/BWLNS2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWLNS2-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

       BWLNS2-INFO-EXIT.
           PERFORM OPEN-BI-RECORD.

      ***************************************
       BORROWER-IO-ROUTINES SECTION.
      ***************************************
       OPEN-BI-RECORD.

      *     IF SCAN-TYPE = 1
            PERFORM OPEN-BI1-FILE.
      *     ELSE
      *        PERFORM OPEN-BI2-FILE.

       START-BI-RECORD.

           MOVE BI-SSNO   TO QBI1-WBEG-SSNO.
           MOVE BI-LNAME  TO QBI2-WBEG-LNAME
           MOVE BI-FNAME  TO QBI2-WBEG-FNAME.

           IF SCAN-TYPE = 1
              MOVE "S" TO PASS-SEL-TYPE
           ELSE
              MOVE "L" TO PASS-SEL-TYPE.

           IF SCAN-TYPE = 1 AND BILNSC-CALLED-FROM = "BISCAN"
              MOVE "S"      TO PASS-SEL-TYPE
              MOVE BI-SSNO  TO QBI1-WBEG-SSNO
                               QBI1-WEND-SSNO
              MOVE 0        TO BI-ACCTNO BI-OWNBR
                               QBI1-WBEG-OWNBR
                               QBI1-WBEG-ACCTNO
              MOVE ALL "9"  TO QBI1-WEND-ACCTNO
                               QBI1-WEND-OWNBR
              MOVE SPACES   TO QBI2-WBEG-LNAME
                               QBI2-WBEG-FNAME.

      *     IF SCAN-TYPE = 1
              PERFORM START-BI1-FILE.
      *     ELSE
      *        PERFORM START-BI2-FILE.

      * READ-BI-RECORD.
      *
      *     IF SCAN-TYPE = 1
      *        PERFORM READ-BI1-FILE-NEXT
      *     ELSE
      *     IF SCAN-TYPE = 2
      *        PERFORM READ-BI2-FILE-NEXT
      *        IF IO-FG = 0 AND SUB = 1 AND CURRENT-PAGE NOT = 1
      *           PERFORM READ-BI2-FILE-NEXT
      *            UNTIL ( BI-SSNO = PAGE-KEY-SSNO(CURRENT-PAGE) AND
      *                    BI-ACCTNO = PAGE-KEY-ACCTNO(CURRENT-PAGE) AND
      *                    BI-OWNBR = PAGE-KEY-OWNBR(CURRENT-PAGE) )
      *                    OR IO-FG = 9.
      
       CLOSE-BI-RECORD.
      *     IF SCAN-TYPE = 1
              PERFORM CLOSE-BI1-FILE.
      *     ELSE
      *        PERFORM CLOSE-BI2-FILE.

      ***********************************
       ARRANGE-BI-KEY SECTION.
      ***********************************
           MOVE 1 TO IX1 IX2.
           MOVE SPACES TO BI-ALP-TABLE.
       ARR-BI-KEY-AGAIN.
           IF VDUTAB(IX1) NOT = ","
              MOVE VDUTAB(IX1) TO BI-ALP-TBL(IX2)
           ELSE
              MOVE 35 TO IX2.
           IF IX1 < 20
              IF IX2 < 70
                 ADD 1 TO IX1
                 ADD 1 TO IX2
                 GO TO ARR-BI-KEY-AGAIN.

      ***********************************
       SEND-SORT-PROMPT SECTION.
      ***********************************
           IF SCAN-TYPE = 1
              MOVE SSNO-PROMPT TO WR-BUF
           ELSE
              MOVE NAME-PROMPT TO WR-BUF.
           MOVE PROMPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/DATER.CPY".

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           DISPLAY BILNSC-SCREEN UPON BILNSC-WINDOW-HANDLE.
           MOVE BILNSC-FORM-FIELDS TO WR-BUF.
           MOVE BILNSC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      ********************************************
       SETUP-LINK-INFO SECTION.
      ********************************************
           MOVE BILNSC-HELP--FILE TO HELP-LINK-FILE.
           MOVE BILNSC-HELP--DIR  TO HELP-LINK-DIR.

      ********************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/BILNSC_EVA.CPY".
              COPY "LIBWI/BISCN2_EVA.CPY".
              COPY "LIBWI/BWLNS2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **********************************
       MISC-PARAGRAPHS SECTION.
      **********************************
       COPY "LIBGB/ACCESS.CPY".
       GET-SP.
           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.
           PERFORM READ-SP1-FILE
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
       GET-LT.
           MOVE LN-OWNBR  TO LTC-BRNO.
           MOVE LN-ACCTNO TO LTC-ACCTNO.
           MOVE 1         TO LTC-SEQNO.
           PERFORM OPEN-LTC1-FILE.
           PERFORM READ-LTC1-FILE.
           PERFORM CLOSE-LTC1-FILE.
       SEND-MORE-INFO.
           MOVE "MORE INFORMATION IS NOT AVAILABLE" TO MESS.
           PERFORM SEND-MESS.
       GET-BR-KEY.
           IF SCAN-TYPE = 1
              MOVE FLDSEL-S-SSNO TO ALP-BI
              PERFORM ALPBI-TO-NUMBI
              MOVE NUM-BI TO BW-SSNO
              INSPECT FLDSEL-S-OWNBR REPLACING LEADING " " BY "0"
              MOVE FLDSEL-S-OWNBR TO BR-NO
           ELSE
              MOVE FLDSEL-N-SSNO TO ALP-BI
              PERFORM ALPBI-TO-NUMBI
              MOVE NUM-BI TO BW-SSNO
              INSPECT FLDSEL-N-OWNBR REPLACING LEADING " " BY "0"
              MOVE FLDSEL-N-OWNBR TO BR-NO.
       GET-BR.
           MOVE 0 TO IO-FG.
           IF BR-NO NOT = HOLD-BR-KEY
              PERFORM OPEN-BR-FILE
              PERFORM READ-BR-FILE
              PERFORM CLOSE-BR-FILE
              IF IO-FG = 0
                 MOVE BR-NO TO HOLD-BR-KEY.

       GET-BW-GLOBAL.
           MOVE 0 TO IO-FG.

           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
       GET-LN-GLOBAL.
           MOVE 0 TO IO-FG.
           MOVE BR-NO           TO LN-OWNBR.
           MOVE FLDSEL-S-ACCTNO TO LN-ACCTNO.
           INSPECT LN-ACCTNO REPLACING ALL " " BY "0".
           IF LN1-KEY NOT = HOLD-LN1-KEY
              PERFORM OPEN-LN1-FILE
              PERFORM READ-LN1-FILE
              PERFORM CLOSE-LN1-FILE
              IF IO-FG = 0
                 MOVE LN1-KEY TO HOLD-LN1-KEY.
       SEND-PHONE.
           MOVE TEL-1 TO TEL-ED1.
           MOVE TEL-2 TO TEL-ED2.
           MOVE TEL-3 TO TEL-ED3.
       SET-BI-LNAME.
           INSPECT MESS REPLACING FIRST "  " BY ",?".
           INSPECT MESS REPLACING FIRST 
             "                                   "
               BY A-NAME.
           INSPECT MESS REPLACING ALL "?" BY " ".
       SET-BI-FNAME.
           INSPECT MESS REPLACING FIRST "  " BY "? ".
           INSPECT MESS REPLACING FIRST
             "                                   "
               BY A-NAME.
           INSPECT MESS REPLACING ALL "?" BY " ".
       SET-CITY-STATE-ZIP.
           MOVE BW-CITY TO MESS.
           INSPECT MESS REPLACING FIRST "  " BY ",?".
           INSPECT MESS REPLACING FIRST "  "
               BY BW-STATE.
           INSPECT MESS REPLACING FIRST "  " BY "? ".
           INSPECT MESS REPLACING FIRST "          "
               BY BW-ZIP.
           INSPECT MESS REPLACING ALL "?" BY " ".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       NUMBI-TO-ALPBI.
           MOVE NUM-BI123 TO ALP-BI123.
           MOVE NUM-BI45 TO ALP-BI45.
           MOVE NUM-BI6789 TO ALP-BI6789.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       ALPBI-TO-NUMBI.
           MOVE ALP-BI123 TO NUM-BI123.
           MOVE ALP-BI45 TO NUM-BI45.
           MOVE ALP-BI6789 TO NUM-BI6789.
           IF NUM-BI NOT NUMERIC
              MOVE 0 TO NUM-BI.

      ****************************************
       IO-ROUTINES SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
      *     PERFORM CLOSE-BI2-FILE.
           PERFORM CLOSE-BI1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLHGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBIGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      * COPY "LIBLP/LPBI2RN.CPY".

      * COPY "LIBLP/LPBI1RN.CPY".
      * COPYMEMBER: LIBLP/LPBI1RN
       LOAD-BI1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BI-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BI-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BI-ALL-BASE
              MOVE BI-ALL-PATH             TO BI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BI-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BI-ALL-BASE
              MOVE BI-ALL-PATH             TO BI-PATH.

      *-----------------------------------------------------------------
       OPEN-BI1-FILE.
           PERFORM LOAD-BI1-FILE.
           PERFORM OPEN-IT.
           MOVE BI-PATH-SQL TO E-FILE.

            IF BI1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BI1_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO BI1-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-BI1-FILE.  *> SSP  BIFILE
           PERFORM START-IT.
           MOVE BI-PATH-SQL TO E-FILE.
           MOVE BI1-KEY TO E-KEYX.

           IF ( BI1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BI1-FILE.

           EXEC SQL SET CONNECTION BI1_CONNECT END-EXEC.

      *     IF @SEL_TYPE = 'S'
      *        SELECT @SQL =	'SELECT TOP 900
      *             BIFILE.BI_SSNO,
      *             BIFILE.BI_ACCTNO,
      *             BIFILE.BI_OWNBR,
      *             BIFILE.BI_LNAME,
      *             BIFILE.BI_FNAME,
      *             CAST(BIFILE.BI_LTOUCH_DATE AS VARCHAR(10)) BI_LTOUCH_DATE,
      *             BIFILE.BI_MAKERCD,
      *             BIFILE.BI_ORGST,
      *             BIFILE.BI_GIFTNET,
      *             CAST(BIFILE.BI_POFFDATE AS VARCHAR(10)) BI_POFFDATE,
      *             CAST(BIFILE.BI_ENTDATE AS VARCHAR(10)) BI_ENTDATE,
      *             BIFILE.BI_SALEFIN,
      *             BIFILE.BI_CONVERCD,
      *             BIFILE.BI_PLCD,
      *             BIFILE.BI_SMALLOAN,
      *             CAST(BIFILE.BI_PLDATE AS VARCHAR(10)) BI_PLDATE
      *          FROM DBO.BIFILE 
      *          WHERE BIFILE.BI_SSNO >= @QBI_WBEG_SSNO
      *          ORDER BY BIFILE.BI_SSNO, BIFILE.BI_ACCTNO, BIFILE.BI_OWNBR'
      *     IF @SEL_TYPE = 'L'
      *        SELECT @SQL =	'SELECT TOP 900 BIFILE.BI_SSNO,
      *                 BIFILE.BI_ACCTNO,
      *                 BIFILE.BI_OWNBR,
      *                 BIFILE.BI_LNAME,
      *                 BIFILE.BI_FNAME,
      *                 CAST(BIFILE.BI_LTOUCH_DATE AS VARCHAR(10)) BI_LTOUCH_DATE,
      *                 BIFILE.BI_MAKERCD,
      *                 BIFILE.BI_ORGST,
      *                 BIFILE.BI_GIFTNET,
      *                 CAST(BIFILE.BI_POFFDATE AS VARCHAR(10)) BI_POFFDATE,
      *                 CAST(BIFILE.BI_ENTDATE AS VARCHAR(10)) BI_ENTDATE,
      *                 BIFILE.BI_SALEFIN,
      *                 BIFILE.BI_CONVERCD,
      *                 BIFILE.BI_PLCD,
      *                 BIFILE.BI_SMALLOAN,
      *                 CAST(BIFILE.BI_PLDATE AS VARCHAR(10)) BI_PLDATE
      *              FROM (SELECT TOP 900
      *                       BIFILE.BI_LNAME,
      *                       BIFILE.BI_FNAME
      *                    FROM DBO.BIFILE
      *                      WHERE BIFILE.BI_LNAME >= @QBI_WBEG_LNAME
      *                       AND  BIFILE.BI_FNAME >= @QBI_WBEG_FNAME
      *                    GROUP BY BIFILE.BI_LNAME, BIFILE.BI_FNAME
      *                    ORDER BY BIFILE.BI_LNAME, BIFILE.BI_FNAME) AS T1
      *                  INNER JOIN DBO.BIFILE BIFILE
      *                    ON BIFILE.BI_LNAME  = T1.BI_LNAME
      *                   AND BIFILE.BI_FNAME = T1.BI_FNAME
      *                 ORDER BY BIFILE.BI_LNAME, BIFILE.BI_FNAME, BIFILE.BI_SSNO, BIFILE.BI_ACCTNO, BIFILE.BI_OWNBR'

           EXEC SQL
            DECLARE BI1_CURSOR CURSOR FOR
                :BI1-RETURN-CODE = 
                 EXEC SSP_BILNSC_BIFILE_SELECT
                  ( :PASS-SEL-TYPE,    :QBI1-WBEG-SSNO,
                    :QBI2-WBEG-LNAME,  :QBI2-WBEG-FNAME)
                END-EXEC.
   
           EXEC SQL
               OPEN BI1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE STAT---GOOD TO BI1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-BI1-FILE-NEXT.   *> SSP  BIFILE
           PERFORM READ-IT.
           MOVE BI-PATH-SQL TO E-FILE.
           MOVE BI1-KEY     TO E-KEYX.
           INITIALIZE QBI-REC.

           EXEC SQL SET CONNECTION BI1_CONNECT END-EXEC.

           IF ( BI1-CURSOR-STAT-GOOD )
              IF SCAN-TYPE = 1
                 EXEC SQL
                    FETCH BI1_CURSOR INTO 
                     :QBI-REC
                 END-EXEC
              ELSE
                 EXEC SQL
                    FETCH BI1_CURSOR INTO 
                     :QBI-REC
                 END-EXEC
              END-IF  
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BI1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BI-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-BI1-FILE.
           PERFORM CLOSE-IT.

           IF ( BI1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BI1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BI1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO BI1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-BI1-CONNECTION.

           IF BI1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION BI1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT BI1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO BI1-CONN-OPEN-SW.

      ****************************************
       END-ROUTINE SECTION.
      ****************************************
           PERFORM CLOSE-FILES.
           PERFORM CLOSE-BI1-CONNECTION.

       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BILNSC-SCREEN.
           DESTROY BILNSC-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
