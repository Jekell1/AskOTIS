      *  A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     SPIRRE.  
      *****************************************************
      * CINDY
      *
      *    CREATES PIPE DELIMITED /USR/EXTRACT/             
      *  REV:   GET THE IRFILE                          
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      *COPY "LIBLP/LPFSLTP.CPY".

           SELECT WK-IRREAD-FILE ASSIGN OUTPUT WK-IRREAD-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.


       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
      *COPY "LIBLP/LPFDLTP.CPY".

      **************************************************
      *   LOAN ASCII FILE    RSZ =  (INCLUDES 0D0A)
      **************************************************
       FD  WK-IRREAD-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-IRREAD-REC.
           03  WK-IRREAD-BYTE        PIC X OCCURS 2000 TIMES.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/SPIRRE S35 03/31/23-09:24:12 >".

      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
      *COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01IR.CPY".
       COPY "LIBLP/LP01IR_SQL.CPY".
       COPY "LIBLP/LPWSIR.CPY".

       01  WK-IRREAD-PATH.
           03  WK-IRREAD-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-IRREAD-FILENAME               PIC X(8) 
                 VALUE "/IRREAD.".                    

           03  WK-IRREAD-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-IRREAD-PATH-YR                PIC 99.
           03  WK-IRREAD-PATH-MO                PIC 99.
           03  WK-IRREAD-PATH-DA                PIC 99.


       01  WS-IRREAD-REC.
           03  WS-IRREAD-SP-KEY                   PIC X(9).
           03  WS-IRREAD-INSRATECD                PIC ZZZZ9.
           03  WS-IRREAD-IR-KEY                   PIC ZZZZ9.
           03  WS-IRREAD-IR-PER-FG                PIC X.
           03  WS-IRREAD-INSRATETBL      OCCURS 120.
               05  WS-IRREAD-IR-INSRATE             PIC ZZZZ.999.
           03 WS-IRREAD-INSRATE-TB REDEFINES 
                                    WS-IRREAD-INSRATETBL.
               05  WS-IRREAD-IR-INSRATE-1           PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-2           PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-3           PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-4           PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-5           PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-6           PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-7           PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-8           PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-9           PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-10          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-11          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-12          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-13          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-14          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-15          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-16          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-17          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-18          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-19          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-20          PIC ZZZ9.999.

               05  WS-IRREAD-IR-INSRATE-21          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-22          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-23          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-24          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-25          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-26          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-27          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-28          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-29          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-30          PIC ZZZ9.999.

               05  WS-IRREAD-IR-INSRATE-31          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-32          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-33          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-34          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-35          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-36          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-37          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-38          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-39          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-40          PIC ZZZ9.999.

               05  WS-IRREAD-IR-INSRATE-41          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-42          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-43          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-44          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-45          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-46          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-47          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-48          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-49          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-50          PIC ZZZ9.999.

               05  WS-IRREAD-IR-INSRATE-51          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-52          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-53          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-54          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-55          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-56          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-57          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-58          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-59          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-60          PIC ZZZ9.999.

               05  WS-IRREAD-IR-INSRATE-61          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-62          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-63          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-64          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-65          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-66          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-67          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-68          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-69          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-70          PIC ZZZ9.999.

               05  WS-IRREAD-IR-INSRATE-71          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-72          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-73          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-74          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-75          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-76          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-77          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-78          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-79          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-80          PIC ZZZ9.999.

               05  WS-IRREAD-IR-INSRATE-81          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-82          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-83          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-84          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-85          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-86          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-87          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-88          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-89          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-90          PIC ZZZ9.999.

               05  WS-IRREAD-IR-INSRATE-91          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-92          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-93          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-94          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-95          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-96          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-97          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-98          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-99          PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-100         PIC ZZZ9.999.

               05  WS-IRREAD-IR-INSRATE-101         PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-102         PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-103         PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-104         PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-105         PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-106         PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-107         PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-108         PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-109         PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-110         PIC ZZZ9.999.

               05  WS-IRREAD-IR-INSRATE-111         PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-112         PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-113         PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-114         PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-115         PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-116         PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-117         PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-118         PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-119         PIC ZZZ9.999.
               05  WS-IRREAD-IR-INSRATE-120         PIC ZZZ9.999.




       01  WK-IRREAD-IFN             PIC X(6) VALUE "IRREAD".

       01  HOLD-SP1-KEY                PIC X(9)     VALUE SPACES.
       01  FILLER            REDEFINES HOLD-SP1-KEY.
           03  HOLD-SP-ORGST           PIC X(2).
           03  HOLD-SP-SPRCLASS        PIC 9(3).
           03  HOLD-SP-SUBCLASS        PIC 9(2).
           03  HOLD-SP-LAWCODE         PIC 9(2).

       01  HOLD-FIL-PATH.
           03  FILLER           PIC X(9).
           03  HOLD-FIL-MACHINE PIC X(2).
           03  FILLER           PIC X(7). 

       01  HOLD-BR-LOAN-NUM-FLAG PIC X.

       01  SUB                  PIC 999.
       01  JUSTIFY-TYPE     PIC X VALUE "L".

       01  TODAYS-DATE      PIC 9(8).
 
       01  WS-BRACCT.
           03  WS-OWNBR     PIC Z(6).
           03  WS-NUMBER    PIC 9(6).
       01  WS-LN-ACCTNO  PIC 9(8).
       01  WS-LN-OWNBR   PIC 9(4).
      *01  WS-IRREAD-IR-KEY         PIC ZZZZ9.

       01  CR-1          PIC X VALUE X"0D".
      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99   COMP VALUE 0.
           03  LN-COUNT         PIC 999  COMP VALUE 99.
           03  LN-WK            PIC 999  COMP VALUE 0.
       01  PAGE-NUMBER          PIC 9(4) COMP VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
 
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".

       01  MESS-LIST            PIC X(5) VALUE "MESS.".
       01  DISPLAY-BUF.
           03  FILLER           PIC X(12) VALUE "PROCESSING..".
           03  DISPLAY-BRNO     PIC Z(4).
           03  FILLER           PIC X(7).
           03  FILLER           PIC X(17) VALUE SPACES.

       01  TOTAL-RECORDS        PIC S9(13)  COMP-3 VALUE 0.
       01  TOTAL-BYPASSED       PIC S9(13)  COMP-3 VALUE 0.
       01  TOTAL-INCLUDED       PIC S9(13)  COMP-3 VALUE 0.
    
    

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".



           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "B".

       01  SV-BEG-BR           PIC 9(4)   VALUE 9999.
       01  SV-END-BR           PIC 9(4)   VALUE 9999.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(8)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(13)    VALUE " ".

       01  HEAD3.
           03  FILLER           PIC X(9)    VALUE "BRANCH NO".
           03  FILLER           PIC X(3)    VALUE SPACES.
           03  FILLER           PIC X(13)   VALUE "TOTAL RECORDS".
           03  FILLER           PIC X(3)    VALUE SPACES.
           03  FILLER           PIC X(12)   VALUE "TOTAL LN AMT".
           03  FILLER           PIC X(3)    VALUE SPACES.
           03  FILLER           PIC X(11)   VALUE "TOTAL FILES".


       01  DETAIL-LINE          PIC X(80) VALUE SPACES.
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z(4).
           03  FILLER           PIC X(2).
           03  D-NUMBER         PIC Z(6).
           03  FILLER           PIC X(2).
           03  D-NAME           PIC X(15).
           03  FILLER           PIC X(2).
           03  D-REPODATE       PIC 99/99/99.
           03  FILLER           PIC X(5).
           03  D-CURBAL         PIC Z(6).99.

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  D-TOTAL-BRNO     PIC Z(4).
           03  FILLER           PIC X(10).
           03  D-TOTAL-REC      PIC Z(7).
           03  FILLER           PIC X(20).
           03  D-TOTAL-AMT      PIC Z(9).99.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(5).
           03  RANGE1           PIC X(10).
           03  RANGE1N REDEFINES RANGE1 PIC 9(10).
           03  RANGE1D REDEFINES RANGE1 PIC ZZ99/99/99.
           03  FILLER           PIC X(2).
           03  RANGE2           PIC X(10).
           03  RANGE2N REDEFINES RANGE2 PIC 9(10).
           03  RANGE2D REDEFINES RANGE2 PIC ZZ99/99/99.

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".

      *COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBGB/GETENVW.CPY".
      *COPY "LIBCL/GLOBRDW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
      *COPY "LIBLP/LPICOMW.CPY".
      *COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
      *COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
      *COPY "LIBLP/LPSRCDW.CPY".
      *COPY "LIBLP/LPUPWKW.CPY".
      *COPY "LIBLP/LPXDTEW.CPY".
      *COPY "LIBLP/LPITRMW.CPY".
      *COPY "LIBLP/LPAMTSW.CPY".
      *COPY "LIBLP/LPLCHGW.CPY".
      *COPY "LIBLP/LPCEPPW.CPY". 
      *COPY "LIBLP/LNCOSTW.CPY".
      *COPY "LIBLP/LPEARNW.CPY".
      *COPY "LIBLP/LPIBPCW.CPY".
      *COPY "LIBLP/LPCDELW.CPY".
      *COPY "LIBLP/LPLCASW.CPY".
      *COPY "LIBLP/LPLCAPW.CPY".
      *COPY "LIBLP/LPPDUEW.CPY".
      *COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUT/TESTR3_DEF.CPY".
       COPY "LIBUT/TESTR3_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/TESTR3_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-IRREAD-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-IR-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               PR-FILE WK-IRREAD-FILE.

       COPY "LIBGB/DECLRP2.CPY".

      *************************************
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TESTR3" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-SP.
           PERFORM READ-SP1-FILE-NEXT.
*L2G       IF IO-BAD 
              GO TO END-ROUTINE.       

           ADD 1 TO TOTAL-RECORDS.
           MOVE SP1-KEY TO HOLD-SP1-KEY.           

           MOVE 1 TO ILEV.
           IF SP-INS-RATE-FILE(ILEV)
              MOVE SPACES TO WS-IRREAD-REC
                             WK-IRREAD-REC 
              PERFORM GET-INS-RATE-FILE 
              PERFORM UPDATE-IRREAD-WORK-REC.  

           MOVE 2 TO ILEV.
           IF SP-INS-RATE-FILE(ILEV)
              MOVE SPACES TO WS-IRREAD-REC
                             WK-IRREAD-REC 
              PERFORM GET-INS-RATE-FILE 
              PERFORM UPDATE-IRREAD-WORK-REC.  

           MOVE 3 TO ILEV.
           IF SP-INS-RATE-FILE(ILEV)
              MOVE SPACES TO WS-IRREAD-REC
                             WK-IRREAD-REC 
              PERFORM GET-INS-RATE-FILE 
              PERFORM UPDATE-IRREAD-WORK-REC.  

           MOVE 4 TO ILEV.
           IF SP-INS-RATE-FILE(ILEV)
              MOVE SPACES TO WS-IRREAD-REC
                             WK-IRREAD-REC 
              PERFORM GET-INS-RATE-FILE 
              PERFORM UPDATE-IRREAD-WORK-REC.  

           MOVE 5 TO ILEV.
           IF SP-INS-RATE-FILE(ILEV)
              MOVE SPACES TO WS-IRREAD-REC
                             WK-IRREAD-REC 
              PERFORM GET-INS-RATE-FILE 
              PERFORM UPDATE-IRREAD-WORK-REC.  

           MOVE 6 TO ILEV.
           IF SP-INS-RATE-FILE(ILEV)
              MOVE SPACES TO WS-IRREAD-REC
                             WK-IRREAD-REC 
              PERFORM GET-INS-RATE-FILE 
              PERFORM UPDATE-IRREAD-WORK-REC.  
     
           MOVE 7 TO ILEV.
           IF SP-INS-RATE-FILE(ILEV)
              MOVE SPACES TO WS-IRREAD-REC
                             WK-IRREAD-REC 
              PERFORM GET-INS-RATE-FILE 
              PERFORM UPDATE-IRREAD-WORK-REC.  

           MOVE 8 TO ILEV.
           IF SP-INS-RATE-FILE(ILEV)
              MOVE SPACES TO WS-IRREAD-REC
                             WK-IRREAD-REC 
              PERFORM GET-INS-RATE-FILE 
              PERFORM UPDATE-IRREAD-WORK-REC.  


           GO TO NEXT-SP.

       END-ROUTINE.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.



      /**********************************
      *   I N I T I A L I Z A T I O N   *
      ***********************************
       INITIALIZATION SECTION.
           MOVE TESTR3-QUEUE-POS TO Q-POS.
           MOVE TESTR3-COPIES-POS TO C-POS.
           MOVE TESTR3-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-SP1-FILE.

           PERFORM OPEN-PR-FILE.
           PERFORM ENABLE-INTERRUPT.


           MOVE 0 TO TOTAL-RECORDS.

           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD  TO TODAYS-DATE.

           MOVE "FIL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO  HOLD-FIL-PATH.
           IF STAT NOT = "00"
              GO TO IO-ERROR. 


           MOVE HOLD-FIL-MACHINE TO WK-IRREAD-MACHINE.

           MOVE TODAYS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY-DD TO WK-IRREAD-PATH-DA.
           MOVE DATE-MMDDYY-MM TO WK-IRREAD-PATH-MO.
           MOVE DATE-MMDDYY-YY TO WK-IRREAD-PATH-YR.

           CALL "C$TOLOWER" USING WK-IRREAD-PATH1,VALUE 12.
           PERFORM OPEN-WK-IRREAD-FILE-OUTPUT.
      *-----------------------------------------------
           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.


      ****************************************
      *    SETUP DATES FOR BATCH PROCESS
      ****************************************
       EO-EXEC-AUDIT SECTION.

      ****************************************
      *       ENTER REPORT PARAMETERS
      ****************************************
       ENTER-PARAMETERS SECTION.
       ENTER-PARM.
      * FOR RE-ENTRY ON F4 FROM ACCEPT.

      *============================================
       EXIT-PARM.
           EXIT.


      ****************************************
      *       DISPLAY REPORT PARAMETERS
      ****************************************
       DISPLAY-PARAMETERS SECTION.

    
       GET-INS-RATE-FILE SECTION.
           IF SP-INSRATECD (ILEV) NOT = IR-KEY
              MOVE SP-INSRATECD(ILEV) TO IR-KEY
              MOVE "CL/IRREAD" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH IR-REC
              CANCEL FORM-PROGX.
      *       IF IR-KEY = 0
      *          GO TO UPDATE-IRREAD-WORK-REC-EXIT.

       UPDATE-IRREAD-WORK-REC SECTION.
              MOVE HOLD-SP1-KEY TO WS-IRREAD-SP-KEY.
              MOVE SP-INSRATECD(ILEV)TO WS-IRREAD-INSRATECD.
              CALL "C$JUSTIFY" USING WS-IRREAD-INSRATECD,
                                         JUSTIFY-TYPE.
              MOVE IR-KEY           TO WS-IRREAD-IR-KEY.
              CALL "C$JUSTIFY" USING WS-IRREAD-IR-KEY,JUSTIFY-TYPE.

              MOVE IR-PER-FG        TO WS-IRREAD-IR-PER-FG.
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 120  
                  MOVE IR-INSRATE(SUB) TO WS-IRREAD-IR-INSRATE(SUB)
                  CALL "C$JUSTIFY" USING WS-IRREAD-IR-INSRATE(SUB)
                                            JUSTIFY-TYPE
              END-PERFORM. 



           STRING
               WS-IRREAD-SP-KEY                   ,"|"
               WS-IRREAD-INSRATECD                ,"|"
               WS-IRREAD-IR-KEY                   ,"|"
               WS-IRREAD-IR-PER-FG                ,"|"
               WS-IRREAD-IR-INSRATE-1             ,"|"
               WS-IRREAD-IR-INSRATE-2             ,"|"
               WS-IRREAD-IR-INSRATE-3             ,"|"
               WS-IRREAD-IR-INSRATE-4             ,"|"
               WS-IRREAD-IR-INSRATE-5             ,"|"
               WS-IRREAD-IR-INSRATE-6             ,"|"
               WS-IRREAD-IR-INSRATE-7             ,"|"
               WS-IRREAD-IR-INSRATE-8             ,"|"
               WS-IRREAD-IR-INSRATE-9             ,"|"
               WS-IRREAD-IR-INSRATE-10            ,"|"      
               WS-IRREAD-IR-INSRATE-11            ,"|"
               WS-IRREAD-IR-INSRATE-12            ,"|"
               WS-IRREAD-IR-INSRATE-13            ,"|"
               WS-IRREAD-IR-INSRATE-14            ,"|"
               WS-IRREAD-IR-INSRATE-15            ,"|"
               WS-IRREAD-IR-INSRATE-16            ,"|"
               WS-IRREAD-IR-INSRATE-17            ,"|"
               WS-IRREAD-IR-INSRATE-18            ,"|"
               WS-IRREAD-IR-INSRATE-19            ,"|"
               WS-IRREAD-IR-INSRATE-20            ,"|"     
               WS-IRREAD-IR-INSRATE-21            ,"|"
               WS-IRREAD-IR-INSRATE-22            ,"|"
               WS-IRREAD-IR-INSRATE-23            ,"|"
               WS-IRREAD-IR-INSRATE-24            ,"|"
               WS-IRREAD-IR-INSRATE-25            ,"|"
               WS-IRREAD-IR-INSRATE-26            ,"|"
               WS-IRREAD-IR-INSRATE-27            ,"|"
               WS-IRREAD-IR-INSRATE-28            ,"|"
               WS-IRREAD-IR-INSRATE-29            ,"|"
               WS-IRREAD-IR-INSRATE-30            ,"|"
               WS-IRREAD-IR-INSRATE-31            ,"|"
               WS-IRREAD-IR-INSRATE-32            ,"|"
               WS-IRREAD-IR-INSRATE-33            ,"|"
               WS-IRREAD-IR-INSRATE-34            ,"|"
               WS-IRREAD-IR-INSRATE-35            ,"|"
               WS-IRREAD-IR-INSRATE-36            ,"|"
               WS-IRREAD-IR-INSRATE-37            ,"|"
               WS-IRREAD-IR-INSRATE-38            ,"|"
               WS-IRREAD-IR-INSRATE-39            ,"|"
               WS-IRREAD-IR-INSRATE-40            ,"|"
               WS-IRREAD-IR-INSRATE-41            ,"|"
               WS-IRREAD-IR-INSRATE-42            ,"|"
               WS-IRREAD-IR-INSRATE-43            ,"|"
               WS-IRREAD-IR-INSRATE-44            ,"|"
               WS-IRREAD-IR-INSRATE-45            ,"|"
               WS-IRREAD-IR-INSRATE-46            ,"|"
               WS-IRREAD-IR-INSRATE-47            ,"|"
               WS-IRREAD-IR-INSRATE-48            ,"|"
               WS-IRREAD-IR-INSRATE-49            ,"|"
               WS-IRREAD-IR-INSRATE-50            ,"|"

               WS-IRREAD-IR-INSRATE-51            ,"|"
               WS-IRREAD-IR-INSRATE-52            ,"|"
               WS-IRREAD-IR-INSRATE-53            ,"|"
               WS-IRREAD-IR-INSRATE-54            ,"|"
               WS-IRREAD-IR-INSRATE-55            ,"|"
               WS-IRREAD-IR-INSRATE-56            ,"|"
               WS-IRREAD-IR-INSRATE-57            ,"|"
               WS-IRREAD-IR-INSRATE-58            ,"|"
               WS-IRREAD-IR-INSRATE-59            ,"|"
               WS-IRREAD-IR-INSRATE-60            ,"|"


               WS-IRREAD-IR-INSRATE-61            ,"|"
               WS-IRREAD-IR-INSRATE-62            ,"|"
               WS-IRREAD-IR-INSRATE-63            ,"|"
               WS-IRREAD-IR-INSRATE-64            ,"|"
               WS-IRREAD-IR-INSRATE-65            ,"|"
               WS-IRREAD-IR-INSRATE-66            ,"|"
               WS-IRREAD-IR-INSRATE-67            ,"|"
               WS-IRREAD-IR-INSRATE-68            ,"|"
               WS-IRREAD-IR-INSRATE-69            ,"|"
               WS-IRREAD-IR-INSRATE-70            ,"|"

               WS-IRREAD-IR-INSRATE-71            ,"|"
               WS-IRREAD-IR-INSRATE-72            ,"|"
               WS-IRREAD-IR-INSRATE-73            ,"|"
               WS-IRREAD-IR-INSRATE-74            ,"|"
               WS-IRREAD-IR-INSRATE-75            ,"|"
               WS-IRREAD-IR-INSRATE-76            ,"|"
               WS-IRREAD-IR-INSRATE-77            ,"|"
               WS-IRREAD-IR-INSRATE-78            ,"|"
               WS-IRREAD-IR-INSRATE-79            ,"|"
               WS-IRREAD-IR-INSRATE-80            ,"|"

               WS-IRREAD-IR-INSRATE-81            ,"|"
               WS-IRREAD-IR-INSRATE-82            ,"|"
               WS-IRREAD-IR-INSRATE-83            ,"|"
               WS-IRREAD-IR-INSRATE-84            ,"|"
               WS-IRREAD-IR-INSRATE-85            ,"|"
               WS-IRREAD-IR-INSRATE-86            ,"|"
               WS-IRREAD-IR-INSRATE-87            ,"|"
               WS-IRREAD-IR-INSRATE-88            ,"|"
               WS-IRREAD-IR-INSRATE-89            ,"|"
               WS-IRREAD-IR-INSRATE-90            ,"|"

               WS-IRREAD-IR-INSRATE-91            ,"|"
               WS-IRREAD-IR-INSRATE-92            ,"|"
               WS-IRREAD-IR-INSRATE-93            ,"|"
               WS-IRREAD-IR-INSRATE-94            ,"|"
               WS-IRREAD-IR-INSRATE-95            ,"|"
               WS-IRREAD-IR-INSRATE-96            ,"|"
               WS-IRREAD-IR-INSRATE-97            ,"|"
               WS-IRREAD-IR-INSRATE-98            ,"|"
               WS-IRREAD-IR-INSRATE-99            ,"|"
               WS-IRREAD-IR-INSRATE-100           ,"|"

               WS-IRREAD-IR-INSRATE-101           ,"|"
               WS-IRREAD-IR-INSRATE-102           ,"|"
               WS-IRREAD-IR-INSRATE-103           ,"|"
               WS-IRREAD-IR-INSRATE-104           ,"|"
               WS-IRREAD-IR-INSRATE-105           ,"|"
               WS-IRREAD-IR-INSRATE-106           ,"|"
               WS-IRREAD-IR-INSRATE-107           ,"|"
               WS-IRREAD-IR-INSRATE-108           ,"|"
               WS-IRREAD-IR-INSRATE-109           ,"|"
               WS-IRREAD-IR-INSRATE-110           ,"|"

               WS-IRREAD-IR-INSRATE-111           ,"|"
               WS-IRREAD-IR-INSRATE-112           ,"|"
               WS-IRREAD-IR-INSRATE-113           ,"|"
               WS-IRREAD-IR-INSRATE-114           ,"|"
               WS-IRREAD-IR-INSRATE-115           ,"|"
               WS-IRREAD-IR-INSRATE-116           ,"|"
               WS-IRREAD-IR-INSRATE-117           ,"|"
               WS-IRREAD-IR-INSRATE-118           ,"|"
               WS-IRREAD-IR-INSRATE-119           ,"|"
               WS-IRREAD-IR-INSRATE-120, CR-1             
                   DELIMITED BY "  " INTO WK-IRREAD-REC.
              PERFORM WRITE-WK-IRREAD-FILE.
              IF IO-BAD
                    GO TO IO-ERROR.
       UPDATE-IRREAD-WORK-REC-EXIT.
           EXIT.

      *=============================================================
       SET-COMMON SECTION.
           MOVE 0 TO WS-OWNBR WS-NUMBER WS-LN-ACCTNO WS-LN-OWNBR.
           MOVE LN-OWNBR   TO WS-OWNBR.
           MOVE LN-ACCTNO  TO WS-NUMBER.
           
           MOVE LN-ACCTNO  TO WS-LN-ACCTNO.
           MOVE LN-OWNBR   TO WS-LN-OWNBR.
         
          

      *=============================================================




      ***************************
       PRINT-RANGE-LINES SECTION.
      ***************************

           MOVE 2 TO LN-FEED.

           MOVE "TOTAL RECORDS READ" TO RANGE-SELECTION.
           MOVE TOTAL-RECORDS TO RANGE1N.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS OUT OF RANGE" TO RANGE-SELECTION.
           MOVE TOTAL-BYPASSED TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS SUBMITTED" TO RANGE-SELECTION.
           MOVE TOTAL-INCLUDED TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ****************************
      *    WRITE DETAIL LINE     *
      ****************************
       WRITE-DETAIL-LINE SECTION.
       WRITE-DET-LINE.
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
       EXIT-WRITE-DETAIL-LINE.
           EXIT.

      *******************************
      *    HEAD PAGE ROUTINE        *
      *******************************
       HEAD-PAGE SECTION.
       HEAD-ROUTINE.
           MOVE "N" TO FIRST-TIME.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.



       COPY "LIBGB/GPENV.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".
      *COPY "LIBGB/AGEING.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
      *COPY "LIBGB/SPPDTH.CPY".
      *COPY "LIBLP/LPICOMM.CPY".
      *COPY "LIBLP/OURSLN.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
      *COPY "LIBLP/YIELDLN.CPY".
      *COPY "LIBLP/LPMDTE.CPY".
      *COPY "LIBLP/LPUPWK.CPY".
      *COPY "LIBLP/LPSRCD.CPY".
       COPY "LIBLP/LPUPER.CPY".
      *COPY "LIBLP/LPXDTE.CPY".
      *COPY "LIBLP/LPITRM.CPY".
      *COPY "LIBLP/LPAMTS.CPY".
      *COPY "LIBLP/LPLCHG.CPY".
      *COPY "LIBLP/LPCEPP.CPY".
      *COPY "LIBLP/LPIBPC.CPY".
      *COPY "LIBLP/LPTBNO.CPY".
      *COPY "LIBLP/LNCOST.CPY".
      *COPY "LIBLP/LPEARN.CPY".
      *COPY "LIBLP/LPCDEL.CPY".
      *COPY "LIBLP/LPLCAS.CPY".
      *COPY "LIBLP/LPLCAP.CPY".
      *COPY "LIBLP/LPSCHD.CPY".
      *COPY "LIBLP/LPPDUE.CPY".



      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO TESTR3-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TESTR3-SYSDATE.
           DISPLAY TESTR3-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE TESTR3-FORM-FIELDS TO WR-BUF. |MJD001
           MOVE TESTR3-FORM-LIST TO WR-LIST. |MJD001
           PERFORM CALL-WRFORM. |MJD001

       FORM-RESET-EXIT.
           EXIT.


      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE TESTR3-HELP--FILE TO HELP-LINK-FILE.
           MOVE TESTR3-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/TESTR3_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      ***********************
       MISC-ROUTINES SECTION.
      ***********************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETPR.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
      *COPY "LIBLP/LPLTPCL.CPY".

      **********************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPIRGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
      **********************
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
      *COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPIRRN.CPY".

       OPEN-WK-IRREAD-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-IRREAD-IFN TO E-FILE.
           OPEN OUTPUT WK-IRREAD-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-IRREAD-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-IRREAD-FILE
              GO TO OPEN-WK-IRREAD-FILE-OUTPUT.
       WRITE-WK-IRREAD-FILE.
           PERFORM WRITE-IT.
           MOVE WK-IRREAD-IFN TO E-FILE.
           WRITE WK-IRREAD-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-IRREAD-FILE.
       CLOSE-WK-IRREAD-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-IRREAD-FILE.



       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY TESTR3-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
