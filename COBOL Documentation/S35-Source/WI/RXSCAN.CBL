      * A30 CONVERSION STARTED - HORRIBLE D-LINE DISPLAY BUFFER MESS
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      RXSCAN.
       DATE-WRITTEN.    112796.
      ******************************************************************
      *
      *                 (WI)
      *
      *  DESCRIPTION:   RESCISSION LOAN INDEX SCAN WINDOW
      *
      * REV:
      * KEC 010511 {PR#00000} PARADATA FINANCIAL [CUSTOMER CONFERENCE]
      *             REMOVED THE F3-ALL ON THE RESCISSION STATUS QUESTION
      *
      * KEC 2017.0313 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          FIXED ALL PAGE-KEY FIELDS TO MATCH RX-REC.
      *
      * BAH 170421 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2018.0514 PR#00000 PARADATA A30 JAY & BETH
      *         REMOVED USE OF ENT-GROUP AND/OR GROUP-FLAG.
      *         THIS WAS DONE IN RELATION TO THE EOBUF EXTERNAL ISSUE
      *         WHICH IF NOT BEING SET WILL CAUSE ISSUES FROM THOSE
      *         BYTE POSITIONS COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THOSE BYTES.
      *
      * AMM 190412 LOCAL TO GLOBAL CHANGES
      *
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
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/RXSCAN.CBL J35 12/21/23-09:26:23 >".

      ******************************************************************
      *
      *    P R O G R A M - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01RX.CPY".
       COPY "LIBLP/LP01RX_SQL.CPY".
       COPY "LIBLP/LPWSRX.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".

       01  RXSCAN-LINES           PIC 99   VALUE 22.
       01  RXSCAN-COLUMNS         PIC 99   VALUE 78.
       01  RXSCAN-BEGIN-Y         PIC 99   VALUE 2.
       01  RXSCAN-BEGIN-X         PIC 99   VALUE 2.

       01  ERRCD                   PIC X VALUE " ".

       01  IO-TYPE                 PIC X(3)         VALUE SPACES.
       01  MSEL-LIST               PIC X(8)         VALUE "MSEL:P.".
       01  ONE                     PIC S9.
       01  RESET-FG                PIC X            VALUE "Y".

      *-----------------------------------------------------------------
      *    MISCELLANOUS SUBSCRIPTS.
      *-----------------------------------------------------------------
       01  SUB                     PIC 999   COMP   VALUE 0.

      *- - - - - - - - - - - - - - - - - - - - - - - -  FLDSEL SUBSCRIPT
       01  SUB-FLDSEL              PIC 9(2)         VALUE ZEROES.

       01  WS-ACCTNO                 PIC 9(8).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FOR GETTING THE ACCTNO ON SELECT ENTRY(FLDSEL-FIELD).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  WS-DETAIL-LINE-TABLE.
           03  WS-RXSCAN-ACCTNO      PIC 9(6)       OCCURS 12 TIMES.
           03  WS-FLDSEL-SUB         PIC 9(2)       VALUE ZEROES.
           03  WS-FLDSEL-SUB-BYTE    REDEFINES WS-FLDSEL-SUB
                                     PIC 9(1)       OCCURS 2 TIMES.
           03  WS-FLDSEL-SUBSCRIPT   PIC X(6).
           03  WS-FLDSEL-SUBS-BYTE   REDEFINES WS-FLDSEL-SUBSCRIPT
                                     PIC X(1)       OCCURS 6 TIMES.

      *=================================================================
      *
      *    FOR BYTE-FIND-LAST; BYTE-INSERT; BYTE-SPLIT.
      *
      *=================================================================
       01  BYTES-CONFIGURATION.
           03  BYTES-SUB-1                   PIC 9(2)   VALUE ZEROES.
           03  BYTES-SUB-2                   PIC 9(2)   VALUE ZEROES.
           03  BYTES-SUB-1-MAX               PIC 9(2)   VALUE ZEROES.
           03  BYTES-SUB-2-MAX               PIC 9(2)   VALUE ZEROES.
           03  BYTES-SUB-INSERT              PIC 9(2)   VALUE ZEROES.
           03  BYTES-SUB-INSERT-MAX          PIC 9(2)   VALUE 80.
           03  BYTES-SUB-LAST                PIC 9(2)   VALUE ZEROES.
           03  BYTES-SUB-LAST-MAX            PIC 9(2)   VALUE 80.
           03  BYTES-SUB-SPLIT               PIC 9(2)   VALUE ZEROES.
           03  BYTES-SUB-SPLIT-MAX           PIC 9(2)   VALUE 80.
           03  BYTES-LAST-BYTE               PIC 9(2)   VALUE ZEROES.

           03  BYTES-FIELD-1.
               05  BYTES-ELE-1               PIC X(1)   OCCURS 80 TIMES.

           03  BYTES-FIELD-2.
               05  BYTES-ELE-2               PIC X(1)   OCCURS 80 TIMES.

           03  BYTES-FIELD-INSERT.
               05  BYTES-ELE-INSERT          PIC X(1)   OCCURS 80 TIMES.

           03  BYTES-FIELD-LAST.
               05  BYTES-ELE-LAST            PIC X(1)   OCCURS 80 TIMES.

           03  BYTES-FIELD-SPLIT.
               05  BYTES-ELE-SPLIT           PIC X(1)   OCCURS 80 TIMES.

      *-----------------------------------------------------------------
      *    M E S S A G E S
      *-----------------------------------------------------------------
       01  MESSAGES-FOR-SEND-MESS.
           03  MESSAGE-STATUS-CODE PIC X(32)        VALUE
               "ENTER ONLY: 'I' OR 'R'".

       01  MESSAGES-FOR-HEADERS.
           03  HEADER-LNAME        PIC X(16)        VALUE
               "LAST NAME       ".
           03  HEADER-ACCTNO       PIC X(16)        VALUE
               "ACCOUNT NUMBER  ".

      *=================================================================
      *
      *    R A N G E
      *
      *=================================================================
       01  RANGE-LIST.
           03  FILLER              PIC X(35)        VALUE
               "DLNO1 DATE1 LASTNAME1 LOAN-AMOUNT1 ".
           03  FILLER              PIC X(25)        VALUE
               "DLNO2 DATE2 LOAN-AMOUNT2 ".
           03  FILLER              PIC X(7)         VALUE
               "STATUS.".

      *-----------------------------------------------------------------
      *01  RANGE-BUF.
      *    03  RG-BEG-DLNO         PIC 9(4).99.
      *    03  RG-BEG-INT-DATE     PIC 99/99/99.
      *    03  RG-BEG-LASTNAME     PIC X(16).
      *    03  RG-BEG-LASTNAMERDF  REDEFINES RG-BEG-LASTNAME.
      *        05  RG-BEG-NUMBER   PIC 9(6).
      *        05  FILLER          PIC X(9).
      *    03  RG-BEG-LOAN-AMOUNT  PIC Z,ZZZ,ZZ9.99-.
      *-       -       -       -       -       -       -       -       -
      *    03  RG-END-DLNO         PIC 9(4).99.
      *    03  RG-END-INT-DATE     PIC 99/99/99.
      *    03  RG-END-LOAN-AMOUNT  PIC Z,ZZZ,ZZ9.99-.
      *-       -       -       -       -       -       -       -       -
      *    03  RG-STATUS           PIC X(1).

      *-----------------------------------------------------------------
      *    I N Q U I R Y - R A N G E S
      *-----------------------------------------------------------------
       01  INQUIRY-RANGES.
           03  BEG-DLNO            PIC 9(4)V99.
           03  END-DLNO            PIC 9(4)V99.
           03  BEG-INT-DATE        PIC 9(8).
           03  END-INT-DATE        PIC 9(8).
           03  BEG-LNAME-ACCTNO    PIC X(1).
           03  BEG-LASTNAME        PIC X(16).
           03  BEG-ACCTNO          PIC 9(8).
           03  BEG-ACCTNOX         REDEFINES BEG-ACCTNO
                                   PIC X(8).
           03  FILLER              REDEFINES BEG-ACCTNO.
               05  BEG-NUMBER      PIC 9(6).
               05  BEG-SEQNO       PIC 9(2).
           03  FILLER              REDEFINES BEG-ACCTNO.
               05  BEG-NUMBERX     PIC X(6).
               05  BEG-SEQNOX      PIC X(2).
           03  BEG-LOAN-AMOUNT     PIC S9(7)V99.
           03  END-LOAN-AMOUNT     PIC S9(7)V99.
           03  BEG-STATUS          PIC X(1).
      *    03  ENT-GROUP           PIC X(4).

      *-----------------------------------------------------------------
      *   R A N G E S - S T A T U S
      *        --->   RSTAT-???-ALL  =  'F3' ALL.
      *        --->   RSTAT-???-DIFF =  BEG & END ARE NOT EQUAL.
      *        --->   RSTAT-???-SAME =  BEG & END ARE EQUAL.
      *        --->   RSTAT-???-NA   =  OPTION NOT AVAILABLE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *        --->   RSTAT-PRT-SETUP-1 = 'ACCTNO' PRINTS FIRST.
      *        --->   RSTAT-PRT-SETUP-2 = 'LNAME' PRINTS FIRST.
      *        --->   RSTAT-PRT-SETUP-3 = 'DLNO' PRINTS FIRST.
      *-----------------------------------------------------------------
       01  RANGES-STATUS.
           03  RSTAT-LOAN-AMT       PIC X(1)        VALUE SPACES.
               88  RSTAT-LOAN-AMT-ALL               VALUE "A".
               88  RSTAT-LOAN-AMT-DIFF              VALUE "D".
               88  RSTAT-LOAN-AMT-SAME              VALUE "S".
           03  RSTAT-DLNO          PIC X(1)         VALUE SPACES.
               88  RSTAT-DLNO-ALL                   VALUE "A".
               88  RSTAT-DLNO-DIFF                  VALUE "D".
               88  RSTAT-DLNO-SAME                  VALUE "S".
           03  RSTAT-LNAME         PIC X(1)         VALUE SPACES.
               88  RSTAT-LNAME-ALL                  VALUE "A".
               88  RSTAT-LNAME-DIFF                 VALUE "D".
               88  RSTAT-LNAME-SAME                 VALUE "S".
           03  RSTAT-INTDATE         PIC X(1)       VALUE SPACES.
               88  RSTAT-INTDATE-ALL                VALUE "A".
               88  RSTAT-INTDATE-DIFF               VALUE "D".
               88  RSTAT-INTDATE-SAME               VALUE "S".
           03  RSTAT-ACCTNO        PIC X(1)         VALUE SPACES.
               88  RSTAT-ACCTNO-ALL                 VALUE "A".
               88  RSTAT-ACCTNO-DIFF                VALUE "D".
               88  RSTAT-ACCTNO-SAME                VALUE "S".
           03  RSTAT-STATUS        PIC X(1)         VALUE SPACES.
               88  RSTAT-STATUS-ALL                 VALUE "A".
               88  RSTAT-STATUS-DIFF                VALUE "D".
               88  RSTAT-STATUS-SAME                VALUE "S".
           03  RSTAT-PRT-SETUP     PIC X(1)         VALUE SPACES.
               88  RSTAT-PRT-SETUP-1                VALUE "1".
               88  RSTAT-PRT-SETUP-2                VALUE "2".
               88  RSTAT-PRT-SETUP-3                VALUE "3".

      *-----------------------------------------------------------------
      *    V A L I D  - R A N G E S
      *-----------------------------------------------------------------
       01  VALID-RANGES.
           03  STATUS-CODE         PIC X(1)         VALUE SPACES.
               88  STATUS-CODE-I                    VALUE "I".
               88  STATUS-CODE-R                    VALUE "R".
               88  STATUS-CODE-ALL                  VALUE "*".
               88  VALID-STATUS-CODE                VALUE "I" "R".

       01  SYSDATE-NUM             PIC 9(8).

      *=================================================================
      *
      *    H E A D E R
      *
      *=================================================================
       01  HEADER-LIST.
           03  FILLER              PIC X(24)        VALUE
               "HEADLINE1:S HEADLINE2:S.".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *        ( 76 BYTE DETAIL-LINE ) * ( 2 TIMES ) = 152 BYTES.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  HEADER-BUF.
           03  HEADER-BUF-1        PIC X(76)        VALUE SPACES.
           03  HEADER-BUF-2        PIC X(76)        VALUE SPACES.

      *-----------------------------------------------------------------
      *    HEADER LINES
      *
      *        HEADER-?-1  = 'ACCTNO' PRINTS FIRST.
      *        HEADER-?-2  = 'LNAME' PRINTS FIRST.
      *        HEADER-?-3  = 'DLNO' PRINTS FIRST.
      *-----------------------------------------------------------------
       01  HEADER-1-1.
           03  FILLER              PIC X(50)        VALUE
               "ACCOUNT                         DEALER  INTEREST  ".
           03  FILLER              PIC X(26)        VALUE
               "S       LOAN              ".

      *-----------------------------------------------------------------
       01  HEADER-2-1.
           03  FILLER              PIC X(50)        VALUE
               "NUMBER  LAST NAME               NUMBER    DATE    ".
           03  FILLER              PIC X(26)        VALUE
               "T      AMOUNT    PROCEEDS ".

      *-----------------------------------------------------------------
       01  HEADER-1-2.
           03  FILLER              PIC X(50)        VALUE
               "              ACCOUNT DEALER  DEALER      S INTERE".
           03  FILLER              PIC X(26)        VALUE
               "ST      LOAN              ".

      *-----------------------------------------------------------------
       01  HEADER-2-2.
           03  FILLER              PIC X(50)        VALUE
               "LAST NAME      NUMBER NUMBER  NAME        T   DATE".
           03  FILLER              PIC X(26)        VALUE
               "       AMOUNT    PROCEEDS ".

      *-----------------------------------------------------------------
       01  HEADER-1-3.
           03  FILLER              PIC X(50)        VALUE
               " DEALER DEALER      S INTEREST      LOAN          ".
           03  FILLER              PIC X(26)        VALUE
               "    ACCOUNT               ".

      *-----------------------------------------------------------------
       01  HEADER-2-3.
           03  FILLER              PIC X(50)        VALUE
               " NUMBER NAME        T   DATE       AMOUNT    PROCE".
           03  FILLER              PIC X(26)        VALUE
               "EDS  NUMBER LAST NAME     ".

      *=================================================================
      *
      *    F L D S E L
      *
      *=================================================================
       01  FLDSEL-STAT             PIC XX.
      *-----------------------------------------------------------------
       01  FLDSEL-LIST.
           03  FILLER              PIC X(40)        VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER              PIC X(8)         VALUE
               "110 120.".
      *-----------------------------------------------------------------
       01  FLDSEL-BUF                               PIC X(76).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  FLDSEL-BUF-1                   REDEFINES FLDSEL-BUF.
           03  FLDSEL-ACCTNO-1                      PIC 9(6).
           03  FILLER                               PIC X(2).
           03  FLDSEL-LASTNAME-1                    PIC X(21).
           03  FILLER                               PIC X(2).
           03  FLDSEL-DLNO-1                        PIC ZZZ9.99.
           03  FILLER                               PIC X(2).
           03  FLDSEL-INT-DATE-1                    PIC 99/99/99.
           03  FILLER                               PIC X(2).
           03  FLDSEL-STATUS-1                      PIC X(1).
           03  FILLER                               PIC X(2).
           03  FLDSEL-LOAN-AMOUNT-1                 PIC ZZZ,ZZ9.99-.
           03  FILLER                               PIC X(1).
           03  FLDSEL-PROCEEDS-1                    PIC ZZZ,ZZ9.99-.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  FLDSEL-BUF-2                   REDEFINES FLDSEL-BUF.
           03  FLDSEL-LASTNAME-2                    PIC X(14).
           03  FILLER                               PIC X(1).
           03  FLDSEL-ACCTNO-2                      PIC 9(6).
           03  FILLER                               PIC X(1).
           03  FLDSEL-DLNO-2                        PIC ZZZ9.99.
           03  FILLER                               PIC X(1).
           03  FLDSEL-DLNAME-2                      PIC X(11).
           03  FILLER                               PIC X(1).
           03  FLDSEL-STATUS-2                      PIC X(1).
           03  FILLER                               PIC X(1).
           03  FLDSEL-INT-DATE-2                    PIC 99/99/99.
           03  FILLER                               PIC X(1).
           03  FLDSEL-LOAN-AMOUNT-2                 PIC ZZZ,ZZ9.99-.
           03  FILLER                               PIC X(1).
           03  FLDSEL-PROCEEDS-2                    PIC ZZZ,ZZ9.99-.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  FLDSEL-BUF-3                   REDEFINES FLDSEL-BUF.
           03  FLDSEL-DLNO-3                        PIC ZZZ9.99.
           03  FILLER                               PIC X(1).
           03  FLDSEL-DLNAME-3                      PIC X(11).
           03  FILLER                               PIC X(1).
           03  FLDSEL-STATUS-3                      PIC X(1).
           03  FILLER                               PIC X(1).
           03  FLDSEL-INT-DATE-3                    PIC 99/99/99.
           03  FILLER                               PIC X(1).
           03  FLDSEL-LOAN-AMOUNT-3                 PIC ZZZ,ZZ9.99-.
           03  FILLER                               PIC X(1).
           03  FLDSEL-PROCEEDS-3                    PIC ZZZ,ZZ9.99-.
           03  FILLER                               PIC X(1).
           03  FLDSEL-ACCTNO-3                      PIC 9(6).
           03  FILLER                               PIC X(1).
           03  FLDSEL-LASTNAME-3                    PIC X(14).

      *-----------------------------------------------------------------
       01  FIRST-FIELD             PIC X(4)         VALUE "010.".
       01  LAST-FIELD              PIC X(4)         VALUE "120.".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    'FLDSEL-FIELD' FIELD IS 2 EXTRA ON PURPOSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  FLDSEL-FIELD            PIC X(6)         VALUE "010.  ".
       01  FLDSEL-FIELDX           REDEFINES FLDSEL-FIELD
                                   PIC X(1)         OCCURS 6 TIMES.

      *=================================================================
      *
      *    D I S P L A Y
      *
      *=================================================================
       01  DISPLAY-LIST.
           03  FILLER              PIC X(40)        VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER              PIC X(23)        VALUE
               "110 120 PGNO ENDMORE:F.".

      *-----------------------------------------------------------------
       01  DISPLAY-BUF.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *        ( 76 BYTE DETAIL-LINE ) * ( 12 TIMES ) = 912 BYTES.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-LINE                PIC X(76) OCCURS 12 TIMES.
      *-       -       -       -       -       -       -       -       -
           03  D-LINE-ACCTNO  REDEFINES D-LINE OCCURS 12 TIMES.
                   07  D-ACCTNO-1                   PIC 9(6).
                   07  FILLER                       PIC X(2).
                   07  D-LASTNAME-1                 PIC X(21).
                   07  FILLER                       PIC X(2).
                   07  D-DLNO-1                     PIC ZZZ9.99.
                   07  FILLER                       PIC X(2).
                   07  D-INT-DATE-1                 PIC 99/99/99.
                   07  FILLER                       PIC X(2).
                   07  D-STATUS-1                   PIC X(1).
                   07  FILLER                       PIC X(2).
                   07  D-LOAN-AMOUNT-1              PIC ZZZ,ZZ9.99-.
                   07  FILLER                       PIC X(1).
                   07  D-PROCEEDS-1                 PIC ZZZ,ZZ9.99-.

      *-       -       -       -       -       -       -       -       -
           03  D-LINE-LASTNAME REDEFINES D-LINE OCCURS 12 TIMES.
                   07  D-LASTNAME-2                 PIC X(14).
                   07  FILLER                       PIC X(1).
                   07  D-ACCTNO-2                   PIC 9(6).
                   07  FILLER                       PIC X(1).
                   07  D-DLNO-2                     PIC ZZZ9.99.
                   07  FILLER                       PIC X(1).
                   07  D-DLNAME-2                   PIC X(11).
                   07  FILLER                       PIC X(1).
                   07  D-STATUS-2                   PIC X(1).
                   07  FILLER                       PIC X(1).
                   07  D-INT-DATE-2                 PIC 99/99/99.
                   07  FILLER                       PIC X(1).
                   07  D-LOAN-AMOUNT-2              PIC ZZZ,ZZ9.99-.
                   07  FILLER                       PIC X(1).
                   07  D-PROCEEDS-2                 PIC ZZZ,ZZ9.99-.

      *-       -       -       -       -       -       -       -       -
           03  D-LINE-DEALER REDEFINES D-LINE OCCURS 12 TIMES.
                   07  D-DLNO-3                     PIC ZZZ9.99.
                   07  FILLER                       PIC X(1).
                   07  D-DLNAME-3                   PIC X(11).
                   07  FILLER                       PIC X(1).
                   07  D-STATUS-3                   PIC X(1).
                   07  FILLER                       PIC X(1).
                   07  D-INT-DATE-3                 PIC 99/99/99.
                   07  FILLER                       PIC X(1).
                   07  D-LOAN-AMOUNT-3              PIC ZZZ,ZZ9.99-.
                   07  FILLER                       PIC X(1).
                   07  D-PROCEEDS-3                 PIC ZZZ,ZZ9.99-.
                   07  FILLER                       PIC X(1).
                   07  D-ACCTNO-3                   PIC 9(6).
                   07  FILLER                       PIC X(1).
                   07  D-LASTNAME-3                 PIC X(14).

      *-       -       -       -       -       -       -       -       -
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

      *-----------------------------------------------------------------
       01  PAGE-KEY-TABLE.
           03  PAGE-KEY-1                  PIC 9(8)          OCCURS 600.
           03  PAGE-KEY-2                                    OCCURS 600.
               05  PAGE-KEY-2-BRNO         PIC 9(4).
               05  PAGE-KEY-2-LNAME        PIC X(35).
               05  PAGE-KEY-2-FNAME        PIC X(35).
           03  PAGE-KEY-3                                    OCCURS 600.
               05  PAGE-KEY-3-BRNO         PIC 9(4).
               05  PAGE-KEY-3-DLNO         PIC 9(4)V99.
               05  PAGE-KEY-3-DLNOX        REDEFINES PAGE-KEY-3-DLNO
                                           PIC X(6).
               05  PAGE-KEY-3-INTDATE      PIC 9(8).
               05  PAGE-KEY-3-LNAME        PIC X(35).
               05  PAGE-KEY-3-FNAME        PIC X(35).

       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 9(4)         VALUE 600.
      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBWI/DLSCANW.CPY".
       COPY "LIBWI/RXSCAN_CMD.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/RXSCAN_DEF.CPY".
       COPY "LIBWI/RXSCAN_WKS.CPY".
       01  RXSCAN-WINDOW-HANDLE    PIC X(10).

      /*****************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/RXSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/RXSCAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME RXSCAN-BUF EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M - M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.
           MOVE " " TO VDU-WIMENU-SELECTION.
           MOVE "RXSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE RXSCAN-BEGIN-Y
                   AT COL  RXSCAN-BEGIN-X
                   SIZE    RXSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   RXSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS RXSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SYSDATE-NUM.

           MOVE "L"    TO BEG-LNAME-ACCTNO.

       MAIN-CONT.

           MOVE SPACES TO HEADER-BUF-1 HEADER-BUF-2.
           MOVE HEADER-BUF TO WR-BUF.
           MOVE HEADER-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE SPACES TO DISPLAY-BUF DISPLAY-BUF.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM REC-INQ-RANGES.

           IF ( IO-TYPE = "END" )
              GO TO END-ROUTINE.

           IF ( IO-TYPE = "CLR" )
              GO TO MAIN-CONT.

           PERFORM SCAN-DISPLAY.

           GO TO MAIN-CONT.

      ******************************************************************
       REC-INQ-RANGES SECTION.
      ******************************************************************

       REC-RANGES.
           MOVE SPACES TO IO-TYPE.

           IF ( RESET-FG = "Y" )
              MOVE SPACES TO WR-BUF
              MOVE RANGE-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE "S  A070ENDMORE." TO VDU.
           MOVE SPACES            TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "Y" TO RESET-FG.

       ENTER-LASTNAME-ACCTNO.

           IF ( BEG-LNAME-ACCTNO = "L" )
              MOVE "S  A160H-NAMEACCT:S." TO VDU
              MOVE HEADER-LNAME           TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A160LASTNAME1."    TO VDU
              MOVE "________________"     TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-LASTNAME-RANGE
           ELSE
              MOVE "S  A160H-NAMEACCT:S." TO VDU
              MOVE HEADER-ACCTNO          TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A160LASTNAME1."    TO VDU
              MOVE "________________"     TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-ACCTNO-RANGE.

       ENTER-LASTNAME-RANGE.

           MOVE BEG-LNAME-MSEL TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A160LASTNAME1." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              GO TO REC-RANGES.

           IF ( VDUSTATUS = "16" )
              MOVE " " TO BEG-LASTNAME.

           IF ( VDUSTATUS = "1:" )
              MOVE "A" TO BEG-LNAME-ACCTNO
              GO TO ENTER-LASTNAME-ACCTNO.

           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.

           IF NOT (VDUSTATUS = "00" OR "16" OR "1D")
              GO TO ENTER-LASTNAME-RANGE.

           MOVE VDUBUF TO BEG-LASTNAME.

           IF ( BEG-LASTNAME = SPACES )
              MOVE "S  A160LASTNAME1."    TO VDU
              MOVE "***** ALL ***** "     TO VDUBUF
              PERFORM SCREENIO
              MOVE "A" TO RSTAT-LNAME
              MOVE "3" TO RSTAT-PRT-SETUP
           ELSE
              MOVE "S" TO RSTAT-LNAME
              MOVE "2" TO RSTAT-PRT-SETUP.

           GO TO BEG-DLNO-RANGE.

       ENTER-ACCTNO-RANGE.

           MOVE BEG-ACCTNO-MSEL TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A160LASTNAME1." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              GO TO REC-RANGES.

           IF ( VDUSTATUS = "1:" )
              MOVE "L" TO BEG-LNAME-ACCTNO
              GO TO ENTER-LASTNAME-ACCTNO.

           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-ACCTNO-RANGE.

           MOVE VDUBUF TO BYTES-FIELD-LAST.
           PERFORM BYTES-FIND-LAST.

           IF ( BYTES-LAST-BYTE > 6 )
              MOVE "INVALID NUMBER ENTERED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ACCTNO-RANGE
           ELSE
           IF ( BYTES-LAST-BYTE = 6 )
              MOVE VDUBUF TO BEG-NUMBERX
              MOVE 1      TO BEG-SEQNO
           ELSE
              PERFORM SET-BYTES-FIELD-1
              MOVE VDUBUF             TO BYTES-FIELD-2
              PERFORM BYTES-INSERT
              MOVE BYTES-FIELD-INSERT TO BEG-NUMBERX
              MOVE 1                  TO BEG-SEQNO.

           IF ( BEG-ACCTNO NOT NUMERIC )
              GO TO ENTER-ACCTNO-RANGE.

           MOVE "S  A160LASTNAME1." TO VDU.
           MOVE BEG-NUMBERX         TO VDUBUF.
           PERFORM SCREENIO.

           IF ( BEG-ACCTNO = ZEROES )
              MOVE "A" TO RSTAT-ACCTNO
           ELSE
              MOVE "D" TO RSTAT-ACCTNO.

           MOVE "1" TO RSTAT-PRT-SETUP.

       BEG-DLNO-RANGE.

           IF ( BEG-LNAME-ACCTNO = "L" )
              IF ( RSTAT-LNAME-SAME )
                 GO TO DEFAULT-REC-RANGES
              ELSE
                 NEXT SENTENCE
           ELSE
              GO TO DEFAULT-REC-RANGES.

           MOVE BEG-DLNO-MSEL TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN042DLNO1." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              GO TO REC-RANGES.

           IF ( VDUSTATUS = "16" )
              GO TO ALL-DLNO.

           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-LASTNAME-ACCTNO.

           IF ( VDUSTATUS = "1<" )
              PERFORM DEALER-SCAN
              IF ( IO-FG = 0 )
                  MOVE "SNNN042DLNO1." TO VDU
                  MOVE DLSCAN-DLNO     TO VDUNUM2
                  PERFORM SCREENIO
                  MOVE DLSCAN-DLNO     TO VDUNUM2
              ELSE
                  GO TO BEG-DLNO-RANGE.

           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO BEG-DLNO-RANGE.

           MOVE VDUNUM2 TO BEG-DLNO.

       END-DLNO-RANGE.

           MOVE END-DLNO-MSEL TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN042DLNO2." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              GO TO REC-RANGES.

           IF ( VDUSTATUS = "1U" )
              GO TO BEG-DLNO-RANGE.

           IF ( VDUSTATUS = "1<" )
              PERFORM DEALER-SCAN
              IF ( IO-FG = 0 )
                  MOVE "SNNN042DLNO2." TO VDU
                  MOVE DLSCAN-DLNO     TO VDUNUM2
                  PERFORM SCREENIO
                  MOVE DLSCAN-DLNO     TO VDUNUM2
              ELSE
                  GO TO END-DLNO-RANGE.

           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO END-DLNO-RANGE.

           MOVE VDUNUM2 TO END-DLNO.

           IF ( BEG-DLNO = ZEROES ) AND ( END-DLNO = ZEROES )
              MOVE "INVALID DEALER RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEG-DLNO-RANGE.

           IF ( BEG-DLNO > END-DLNO )
              MOVE "INVALID DEALER RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEG-DLNO-RANGE.

           IF ( BEG-DLNO = END-DLNO )
              MOVE "S" TO RSTAT-DLNO
           ELSE
              MOVE "D" TO RSTAT-DLNO.

           GO TO BEG-INT-DATE-RANGE.

       ALL-DLNO.

           MOVE "SNNN042DLNO1." TO VDU.
           MOVE ZEROES          TO VDUNUM2 BEG-DLNO.
           PERFORM SCREENIO.

           MOVE "SNNN042DLNO2." TO VDU.
           MOVE 9999.99         TO VDUNUM2 END-DLNO.
           PERFORM SCREENIO.

           MOVE "A" TO RSTAT-DLNO.

       BEG-INT-DATE-RANGE.

           MOVE BEG-INTDATE-MSEL TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              GO TO REC-RANGES.
           IF ( VDUSTATUS = "16" )
              GO TO ALL-INT-DATES.
           IF ( VDUSTATUS = "1U" )
              GO TO END-DLNO-RANGE.
           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO BEG-INT-DATE-RANGE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-INT-DATE.

           IF ( VDUNUM0 = 0 )
              MOVE "S  A080DATE1." TO VDU 
              MOVE SYSDATE-NUM     TO BEG-INT-DATE DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO.

       END-INT-DATE-RANGE.

           MOVE ZEROES         TO END-INT-DATE.
           MOVE END-INTDATE-MSEL TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              GO TO REC-RANGES.
           IF ( VDUSTATUS = "1U" )
              GO TO BEG-INT-DATE-RANGE.
           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO END-INT-DATE-RANGE.

           MOVE VDU-DATE-YYYYMMDD TO END-INT-DATE.

           IF ( VDUNUM0 = ZEROES )
              MOVE "S  A080DATE2." TO VDU
              MOVE SYSDATE-NUM     TO END-INT-DATE DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY     TO VDU-DATE-EDIT
              PERFORM SCREENIO.

           IF END-INT-DATE < BEG-INT-DATE
              MOVE "INVALID STATUS DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEG-INT-DATE-RANGE.

           IF END-INT-DATE = BEG-INT-DATE
              MOVE "S" TO RSTAT-INTDATE
           ELSE
              MOVE "D" TO RSTAT-INTDATE.

           GO TO BEG-LOAN-AMOUNT-RANGE.

       ALL-INT-DATES.

           MOVE EXT-JULIAN-BEG TO BEG-INT-DATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE "S  A000DATE1." TO VDU.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE EXT-JULIAN-END TO END-INT-DATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE "S  A000DATE2." TO VDU.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "A" TO RSTAT-INTDATE.

       BEG-LOAN-AMOUNT-RANGE.

           MOVE BEG-LOAN-AMOUNT-MSEL TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  S072LOAN-AMOUNT1." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              GO TO REC-RANGES.

           IF ( VDUSTATUS = "16" )
              GO TO ALL-LOAN-AMOUNT.

           IF ( VDUSTATUS = "1U" )
              GO TO END-INT-DATE-RANGE.

           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO BEG-LOAN-AMOUNT-RANGE.

           MOVE VDUNUM2 TO BEG-LOAN-AMOUNT.

       END-LOAN-AMOUNT-RANGE.

           MOVE END-LOAN-AMOUNT-MSEL TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  S072LOAN-AMOUNT2." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              GO TO REC-RANGES.

           IF ( VDUSTATUS = "1U" )
              GO TO BEG-LOAN-AMOUNT-RANGE.

           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO END-LOAN-AMOUNT-RANGE.

           MOVE VDUNUM2 TO END-LOAN-AMOUNT.

           IF (BEG-LOAN-AMOUNT = ZEROES) AND (END-LOAN-AMOUNT = ZEROES)
              MOVE "INVALID LOAN AMOUNT RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEG-LOAN-AMOUNT-RANGE.

           IF ( BEG-LOAN-AMOUNT > END-LOAN-AMOUNT )
              MOVE "INVALID LOAN AMOUNT RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEG-LOAN-AMOUNT-RANGE.

           IF ( BEG-LOAN-AMOUNT = END-LOAN-AMOUNT )
              MOVE "S" TO RSTAT-LOAN-AMT
           ELSE
              MOVE "D" TO RSTAT-LOAN-AMT.

           GO TO ENTER-STATUS.

       ALL-LOAN-AMOUNT.

           MOVE "S  S072LOAN-AMOUNT1." TO VDU.
           MOVE ZEROES                 TO VDUNUM2 BEG-LOAN-AMOUNT.
           PERFORM SCREENIO.

           MOVE "S  S072LOAN-AMOUNT2." TO VDU.
           MOVE 9999999.99             TO VDUNUM2 END-LOAN-AMOUNT.
           PERFORM SCREENIO.

           MOVE "A" TO RSTAT-LOAN-AMT.

       ENTER-STATUS.

           MOVE BEG-STATUS-MSEL TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A010STATUS." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              GO TO REC-RANGES.

           IF ( VDUSTATUS = "1U" )
              GO TO END-LOAN-AMOUNT-RANGE.

           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-STATUS.

           MOVE VDUBUF TO BEG-STATUS STATUS-CODE.

           IF NOT ( VALID-STATUS-CODE )
              MOVE MESSAGE-STATUS-CODE TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-STATUS.

           MOVE "S" TO RSTAT-STATUS.
           GO TO END-REC-RANGES.

       ALL-STATUS.

           MOVE "S  A010STATUS." TO VDU.
           MOVE "*"              TO VDUBUF BEG-STATUS.
           PERFORM SCREENIO.

           MOVE "A" TO RSTAT-STATUS.

           GO TO END-REC-RANGES.

      * DEFAULT TO ALL OF THE FOLLOWING BECAUSE WE WANT ALL NAMES
      * OR JUST LOOKING FOR A SOCIAL SECURITY NUMBER.

       DEFAULT-REC-RANGES.
           PERFORM ALL-DLNO.
           PERFORM ALL-INT-DATES.
           PERFORM ALL-LOAN-AMOUNT.
           PERFORM ALL-STATUS.

       END-REC-RANGES.
           EXIT.

      ******************************************************************
       SCAN-DISPLAY SECTION.
      ******************************************************************

           PERFORM OPEN-RX1-FILE.

           MOVE RX-PATH-OWNBR TO RX-BRNO.

           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1           TO PAGE-COUNT ONE.
           MOVE 0           TO CURRENT-PAGE.

       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.

           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

       CONT-BUILD-PAGENO.
           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1 TO SUB.

           IF ( RSTAT-PRT-SETUP-1 )
              MOVE PAGE-KEY-1(CURRENT-PAGE) TO RX-ACCTNO
              IF ( RX1-KEY = SPACES )
                 INITIALIZE RX-ACCTNO
              END-IF
              PERFORM START-RX1-FILE
           ELSE
           IF ( RSTAT-PRT-SETUP-2 )
              MOVE PAGE-KEY-2-BRNO(CURRENT-PAGE) TO RX-BRNO
              MOVE PAGE-KEY-2-LNAME(CURRENT-PAGE) TO RX-LNAME
              MOVE PAGE-KEY-2-FNAME(CURRENT-PAGE) TO RX-FNAME
              PERFORM START-RX2-FILE
           ELSE
              IF ( PAGE-KEY-3-DLNOX(CURRENT-PAGE) = SPACES )
                 MOVE ZEROES TO PAGE-KEY-3-DLNO(CURRENT-PAGE)
              END-IF
              MOVE PAGE-KEY-3-BRNO(CURRENT-PAGE)  TO RX-BRNO
              MOVE PAGE-KEY-3-DLNO(CURRENT-PAGE)  TO RX-DLNO
              MOVE PAGE-KEY-3-INTDATE(CURRENT-PAGE) TO RX-INTDATE
              MOVE PAGE-KEY-3-LNAME(CURRENT-PAGE) TO RX-LNAME
              MOVE PAGE-KEY-3-FNAME(CURRENT-PAGE) TO RX-FNAME
              PERFORM START-RX3-FILE.

       BUILD-PAGENO.

           IF ( RSTAT-PRT-SETUP-1 )
              PERFORM READ-RX1-FILE-NEXT
           ELSE
           IF ( RSTAT-PRT-SETUP-2 )
              PERFORM READ-RX2-FILE-NEXT
           ELSE
              PERFORM READ-RX3-FILE-NEXT.

           IF ( IO-FG NOT = 0 ) OR ( RX-BRNO NOT = RX-PATH-OWNBR )
              GO TO DISPLAY-PAGE.

           IF ( BEG-LNAME-ACCTNO = "L" )
              IF NOT ( RSTAT-LNAME-ALL )
                 IF ( RX-LNAME < BEG-LASTNAME )
                    GO TO BUILD-PAGENO.

           IF ( BEG-LNAME-ACCTNO = "A" )
              IF NOT ( RSTAT-ACCTNO-ALL )
                 MOVE RX-ACCTNO TO WS-ACCTNO
                 IF NOT ( WS-ACCTNO >= BEG-ACCTNO )
                    GO TO BUILD-PAGENO.

           IF NOT ( RSTAT-DLNO-ALL )
              IF ( RX-DLNO < BEG-DLNO ) OR ( RX-DLNO > END-DLNO )
                 GO TO BUILD-PAGENO.

           IF NOT ( RSTAT-LOAN-AMT-ALL )
              IF ( RX-LOANAMT < BEG-LOAN-AMOUNT ) OR
                 ( RX-LOANAMT > END-LOAN-AMOUNT )
                 GO TO BUILD-PAGENO.

           IF NOT ( RSTAT-STATUS-ALL )
              IF NOT ( RX-STATUS = BEG-STATUS )
                 GO TO BUILD-PAGENO.

           IF NOT ( RSTAT-INTDATE-ALL )
              IF RX-INTDATE < BEG-INT-DATE
                 GO TO BUILD-PAGENO.

           IF NOT ( RSTAT-INTDATE-ALL )
              IF RX-INTDATE > END-INT-DATE
                 GO TO BUILD-PAGENO.

           IF ( SUB > 12 )
              GO TO DISPLAY-PAGE.

       MOVE-FIELDS.

           MOVE RX-DLNO TO DL-DLNO.
           PERFORM GET-DL.

           IF ( RSTAT-PRT-SETUP-1 )
              MOVE RX-DLNO           TO D-DLNO-1(SUB)
              MOVE RX-INTDATE        TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY       TO D-INT-DATE-1(SUB)
              MOVE RX-PROCEEDS       TO D-PROCEEDS-1(SUB)
              STRING RX-LNAME,", ",RX-FNAME
                     DELIMITED BY "   " INTO D-LASTNAME-1(SUB)
              MOVE RX-ACCTNO         TO WS-ACCTNO
              MOVE WS-ACCTNO         TO D-ACCTNO-1(SUB)
              MOVE RX-LOANAMT        TO D-LOAN-AMOUNT-1(SUB)
              MOVE RX-STATUS         TO D-STATUS-1(SUB)
           ELSE
           IF ( RSTAT-PRT-SETUP-2 )
              MOVE RX-DLNO           TO D-DLNO-2(SUB)
              MOVE RX-INTDATE        TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY       TO D-INT-DATE-2(SUB)
              MOVE RX-PROCEEDS       TO D-PROCEEDS-2(SUB)
              STRING RX-LNAME,", ",RX-FNAME
                     DELIMITED BY "   " INTO D-LASTNAME-2(SUB)
              MOVE DL-SHORT          TO D-DLNAME-2(SUB)
              MOVE RX-LOANAMT        TO D-LOAN-AMOUNT-2(SUB)
              MOVE RX-STATUS         TO D-STATUS-2(SUB)
              MOVE RX-ACCTNO         TO WS-ACCTNO
              MOVE WS-ACCTNO         TO D-ACCTNO-2(SUB)
           ELSE
              MOVE RX-DLNO           TO D-DLNO-3(SUB)
              MOVE RX-INTDATE        TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY       TO D-INT-DATE-3(SUB)
              MOVE RX-PROCEEDS       TO D-PROCEEDS-3(SUB)
              STRING RX-LNAME,", ",RX-FNAME
                     DELIMITED BY "   " INTO D-LASTNAME-3(SUB)
              MOVE DL-SHORT          TO D-DLNAME-3(SUB)
              MOVE RX-LOANAMT        TO D-LOAN-AMOUNT-3(SUB)
              MOVE RX-STATUS         TO D-STATUS-3(SUB)
              MOVE RX-ACCTNO         TO WS-ACCTNO
              MOVE WS-ACCTNO         TO D-ACCTNO-3(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

       DISPLAY-PAGE.

           IF ( IO-FG NOT = 0 )
              IF ( SUB = 1 )
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-MESS
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO END-INQUIRY-MODULE.

           MOVE "MORE..." TO D-END-MORE.

           IF ( CURRENT-PAGE = PAGE-COUNT )
              IF ( IO-FG = 0 )
                 IF ( PAGE-COUNT < MAX-PAGES )
                    ADD 1 TO PAGE-COUNT
                    PERFORM MOVE-KEY-TO-PAGE-KEY
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.

           MOVE CURRENT-PAGE TO D-PAGENO.
           PERFORM SCAN-SETUP-HEADER.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SELECT-FIELD-FROM-DISPLAY.
           MOVE BEG-SELECTION-MSEL TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM CALL-FLDSEL.

      * TEST UP ARROW:
           IF ( FLDSEL-STAT = "1U" )
              IF ( CURRENT-PAGE = 1 )
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE UP:
           IF ( FLDSEL-STAT = "1P" )
              IF ( CURRENT-PAGE = 1 )
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      
      * TEST PAGE DOWN OR DOWN ARROW:
           IF ( FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D" )
              IF ( CURRENT-PAGE NOT < PAGE-COUNT )
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      
      * TEST HOME:
           IF ( FLDSEL-STAT = "1H" )
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

           IF ( FLDSEL-STAT = "1>" )
              MOVE ZEROES TO RXSCAN-BEG-ACCTNO
              GO TO END-ROUTINE.

           IF ( FLDSEL-STAT = "10" )
              MOVE "Y" TO RESET-FG
              GO TO END-INQUIRY-MODULE.
      
           IF NOT ( FLDSEL-STAT = "00" )
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY
           ELSE
           IF ( RXSCAN-RETURN = "Y" )
              IF (  RSTAT-PRT-SETUP-1                    ) AND
                 (( FLDSEL-STATUS-1 = RXSCAN-RETURN-TYPE ) OR
                  ( RXSCAN-RETURN-TYPE-ALL               ))
                 MOVE FLDSEL-ACCTNO-1 TO RXSCAN-BEG-ACCTNO
                 GO TO END-ROUTINE
              ELSE
              IF (  RSTAT-PRT-SETUP-2                    ) AND
                 (( FLDSEL-STATUS-2 = RXSCAN-RETURN-TYPE ) OR
                  ( RXSCAN-RETURN-TYPE-ALL               ))
                 MOVE FLDSEL-ACCTNO-2 TO RXSCAN-BEG-ACCTNO
                 GO TO END-ROUTINE
              ELSE
              IF (  RSTAT-PRT-SETUP-3                    ) AND
                 (( FLDSEL-STATUS-3 = RXSCAN-RETURN-TYPE ) OR
                  ( RXSCAN-RETURN-TYPE-ALL               ))
                 MOVE FLDSEL-ACCTNO-3 TO RXSCAN-BEG-ACCTNO
                 GO TO END-ROUTINE.

           GO TO SELECT-FIELD-FROM-DISPLAY.

       SCAN-SETUP-HEADER.

      * TO PRINT DETAIL-LINE'S HEADER ON THE SCREEN.

           IF ( RSTAT-PRT-SETUP-1 )
              MOVE HEADER-1-1 TO HEADER-BUF-1
              MOVE HEADER-2-1 TO HEADER-BUF-2
           ELSE
           IF ( RSTAT-PRT-SETUP-2 )
              MOVE HEADER-1-2 TO HEADER-BUF-1
              MOVE HEADER-2-2 TO HEADER-BUF-2
           ELSE
              MOVE HEADER-1-3 TO HEADER-BUF-1
              MOVE HEADER-2-3 TO HEADER-BUF-2.

           MOVE HEADER-BUF TO WR-BUF.
           MOVE HEADER-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       MOVE-KEY-TO-PAGE-KEY.

           IF ( RSTAT-PRT-SETUP-1 )
              MOVE RX-ACCTNO TO PAGE-KEY-1(PAGE-COUNT)
           ELSE
           IF ( RSTAT-PRT-SETUP-2 )
              MOVE RX-BRNO TO PAGE-KEY-2-BRNO(PAGE-COUNT)
              MOVE RX-LNAME TO PAGE-KEY-2-LNAME(PAGE-COUNT)
              MOVE RX-FNAME TO PAGE-KEY-2-FNAME(PAGE-COUNT)
           ELSE
              MOVE RX-BRNO TO PAGE-KEY-3-BRNO(PAGE-COUNT)
              MOVE RX-DLNO TO PAGE-KEY-3-DLNO(PAGE-COUNT)
              MOVE RX-INTDATE TO PAGE-KEY-3-INTDATE(PAGE-COUNT)
              MOVE RX-LNAME TO PAGE-KEY-3-LNAME(PAGE-COUNT)
              MOVE RX-FNAME TO PAGE-KEY-3-FNAME(PAGE-COUNT).

       END-INQUIRY-MODULE.
           EXIT.

      ******************************************************************
       DEALER-SCAN SECTION.
      ******************************************************************
           MOVE 0          TO DLSCAN-NUM.
           MOVE "WI/DLSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH DLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( DLSCAN-STATUS NOT = "00" ) OR ( DLSCAN-NUM = " " )
               MOVE 9 TO IO-FG
               GO TO EXIT-DEALER-SCAN.

           MOVE 0 TO IO-FG.

       EXIT-DEALER-SCAN.
           EXIT.

      ******************************************************************
      *
      *    B Y T E S - F I N D - L A S T  ( NEED PIC-BYTES )
      *
      *-----------------------------------------------------------------
      *  IN  : BYTES-FIELD-LAST
      *  OUT : BYTES-FIELD-LAST, BYTES-LAST-BYTE.
      *  DESC: THIS FINDS THE LAST BYTE IN A TABLE OF 80
      *        (BYTES-FIELD-LAST).
      ******************************************************************
       BYTES-FIND-LAST SECTION.

           MOVE 80 TO BYTES-SUB-LAST.

       BYTES-FIND-LAST-NEXT.

           IF ( BYTES-SUB-LAST > ZEROES )
              IF ( BYTES-ELE-LAST(BYTES-SUB-LAST) = SPACES )
                 SUBTRACT 1 FROM BYTES-SUB-LAST
                 GO TO BYTES-FIND-LAST-NEXT.

           MOVE BYTES-SUB-LAST TO BYTES-LAST-BYTE.

       EXIT-BYTES-FIND-LAST.
           EXIT.

      ******************************************************************
      *
      *    B Y T E S - I N S E R T  ( NEED PIC-BYTES )
      *
      *-----------------------------------------------------------------
      *  IN  : BYTES-FIELD-1, BYTES-FIELD-2
      *  OUT : BYTES-FIELD-INSERT.
      *  DESC: THIS FINDS THE LAST BYTE IN A BYTE STRING OF 80
      *        (BYTES-FIELD-1), THEN INSERTS ANOTHER BYTE STRING
      *        (BYTES-FIELD-2) TOGETHER INTO A NEW BYTE STRING
      *        (BYTES-FIELD-INSERT).
      ******************************************************************
       BYTES-INSERT SECTION.

           MOVE BYTES-FIELD-1    TO BYTES-FIELD-LAST.
           PERFORM BYTES-FIND-LAST.
           MOVE BYTES-FIELD-LAST TO BYTES-FIELD-INSERT.
           MOVE BYTES-LAST-BYTE  TO BYTES-SUB-INSERT.
           MOVE ZEROES           TO BYTES-SUB-2.

       INSERT-BYTE-STRING-NEXT.

           ADD 1 TO BYTES-SUB-2 BYTES-SUB-INSERT.

           IF ( BYTES-SUB-INSERT > ZEROES ) AND
              ( BYTES-SUB-INSERT < 81 )
              IF ( BYTES-ELE-2(BYTES-SUB-2) NOT = SPACES )
                 MOVE BYTES-ELE-2(BYTES-SUB-2) TO
                      BYTES-ELE-INSERT(BYTES-SUB-INSERT)
                 GO TO INSERT-BYTE-STRING-NEXT.

       EXIT-INSERT-BYTE-STRING.
           EXIT.

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY RXSCAN-SCREEN UPON RXSCAN-WINDOW-HANDLE.
           MOVE RXSCAN-FORM-FIELDS TO WR-BUF.
           MOVE RXSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE RXSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE RXSCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/RXSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *-----------------------------------------------------------------
       GET-DL.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           IF ( IO-FG NOT = ZEROES )
              MOVE "*NOT FOUND*" TO DL-SHORT.
           PERFORM CLOSE-DL1-FILE.

      *-----------------------------------------------------------------
       GET-RXSCAN-ACCTNO.
           MOVE FLDSEL-FIELD                  TO WS-FLDSEL-SUBSCRIPT.
           MOVE WS-FLDSEL-SUBS-BYTE(1)        TO WS-FLDSEL-SUB-BYTE(1).
           MOVE WS-FLDSEL-SUBS-BYTE(2)        TO WS-FLDSEL-SUB-BYTE(2).
           MOVE WS-RXSCAN-ACCTNO(WS-FLDSEL-SUB) TO RXSCAN-BEG-ACCTNO.


      *-----------------------------------------------------------------
       SET-BYTES-FIELD-1.
           IF ( BYTES-LAST-BYTE = 0 )
              MOVE "000000" TO BYTES-FIELD-1
           ELSE
           IF ( BYTES-LAST-BYTE = 1 )
              MOVE "00000" TO BYTES-FIELD-1
           ELSE
           IF ( BYTES-LAST-BYTE = 2 )
              MOVE "0000" TO BYTES-FIELD-1
           ELSE
           IF ( BYTES-LAST-BYTE = 3 )
              MOVE "000" TO BYTES-FIELD-1
           ELSE
           IF ( BYTES-LAST-BYTE = 4 )
              MOVE "00" TO BYTES-FIELD-1
           ELSE
           IF ( BYTES-LAST-BYTE = 5 )
              MOVE "0" TO BYTES-FIELD-1.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      /*****************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-RX3-FILE.
           PERFORM CLOSE-RX2-FILE.
           PERFORM CLOSE-RX1-FILE.
           PERFORM CLOSE-DL1-FILE.
       COPY "LIBLP/LPRXGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPRX1RN.CPY".
       COPY "LIBLP/LPRX2RN.CPY".
       COPY "LIBLP/LPRX3RN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY RXSCAN-SCREEN.
           DESTROY RXSCAN-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
