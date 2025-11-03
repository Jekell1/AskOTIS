      * COPYMEMBER: LIBSP/SPMEMOW
      ******************************************************************
      *
      *    M E M O   E N T R Y   W O R K E R S
      *
      *-----------------------------------------------------------------
      * REV:
      * GLF 101698 ADDED NOTES MEMOS STUFF
      * KEC 110598 CHANGED 'HOLD' DATES FROM 9(6) TO 9(8) TO BE
      *             YEAR 2000 COMPATIBLE.
      *            ADDED MEMO-HOLD-SYS-YYYYMMDD.
      * BLV 101900 ADDED RV-MEMO-COUNTER FOR REVERSAL MEMOS
      * BLM 101109 ADDED RG-MEMO, USERID & SHORTER MEMO, REGENCY PR# 3682
      * 
      * KEC 2015.1104 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED SEQ FOR VM-SEQ FROM 2 TO 3.
      * 
      * KEC 2015.1209 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED SEQ FOR NM-SEQ FROM 2 TO 3 BYTES.
      *         CHANGED THESE FIELDS/SEQ FROM 2 TO 3 BYTES:
      *            SAVE-BEG-LM-SEQ
      *            SAVE-END-LM-SEQ
      *            SAVE-BEG-NM-SEQ
      *            SAVE-END-NM-SEQ
      *            MEMO-SEQ
      *
      * KEC 2015.1218 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED SEQ FOR IM-SEQ FROM 2 TO 3 BYTES.
      *
      * KEC 2016.0506 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED AM-APPNO TO AM-APP-NUMBER TO MATCH MA1-KEY.
      *
      * KEC 2017.0310 {PR#05265} FIRST METRO <A15> [JUDY REIFSTECK]
      *         ADDED FMETRO-MEMO FIELD.
      *
      * KEC 2018-0823 LOAN APP MODULE REMOVAL FROM A30 [LARM30]
      * BAH 2018.0925 ADDED BM-BRNO
      * BAH 2018.1015 ADDED DM-BRNO
      * BAH 2018.1127 ADDED LM-BRNO
      *
      ******************************************************************
       01  MEMO-HOLD-SYS         PIC 9(8)   COMP.
       01  MEMO-LINE             PIC 99     VALUE 0.
       01  MEMO-MAX              PIC 99     VALUE 8.
       01  MEMO-IO-FG            PIC 9      VALUE 0.
       01  MEMO-AUDIT-FLAG       PIC X.
       01  MEMO-PASSWORD-00-OK   PIC X      VALUE "N".
       01  MEMO-PASSWORD-01-OK   PIC X      VALUE "N".
       01  MEMO-PASSWORD-02-OK   PIC X      VALUE "N".
       01  MEMO-VERIFY-PASSWORD  PIC X      VALUE "N".
       01  DELE-VERIFY-PASSWORD  PIC X      VALUE "N".
       01  MEMO-IS-TODAY         PIC X.
       01  HOLD-MEMO-TYPE        PIC XX     VALUE "  ".
       01  MEMO-WHICH            PIC X(22).
       01  RV-MEMO-COUNTER       PIC 9(04)  VALUE 0.
       01  HEAD-3.
           03  FILLER        PIC X(42) VALUE
           "MEMO_DATE  SEQ  TEXT----------------------".
           03  FILLER        PIC X(34) VALUE
           "----------------------------------".

       01  LM-MEMO-HOLD-KEY.
           03  LM-MEMO-HOLD-BRNO   PIC 9(4).
           03  LM-MEMO-HOLD-ACCTNO PIC 9(8).
           03  LM-MEMO-HOLD-DATE   PIC 9(8).
           03  LM-MEMO-HOLD-SEQ    PIC 9(3).

       01  LM-MEMO-LINE1-KEY.
           03  LM-MEMO-LINE1-BRNO   PIC 9(4).
           03  LM-MEMO-LINE1-ACCTNO PIC 9(8).
           03  LM-MEMO-LINE1-DATE   PIC 9(8).
           03  LM-MEMO-LINE1-SEQ    PIC 9(3).

       01  LM-MEMO-LAST-LINE-KEY.
           03  LM-MEMO-LAST-LINE-BRNO   PIC 9(4).
           03  LM-MEMO-LAST-LINE-ACCTNO PIC 9(8).
           03  LM-MEMO-LAST-LINE-DATE   PIC 9(8).
           03  LM-MEMO-LAST-LINE-SEQ    PIC 9(3).

       01  BM-MEMO-HOLD-KEY.
           03  BM-MEMO-HOLD-BRNO   PIC 9(4).
           03  BM-MEMO-HOLD-SSNO   PIC 9(9).
           03  BM-MEMO-HOLD-DATE   PIC 9(8).
           03  BM-MEMO-HOLD-SEQ    PIC 9(3).

       01  BM-MEMO-LINE1-KEY.
           03  BM-MEMO-LINE1-BRNO   PIC 9(4).
           03  BM-MEMO-LINE1-SSNO   PIC 9(9).
           03  BM-MEMO-LINE1-DATE   PIC 9(8).
           03  BM-MEMO-LINE1-SEQ    PIC 9(3).

       01  BM-MEMO-LAST-LINE-KEY.
           03  BM-MEMO-LAST-LINE-BRNO   PIC 9(4).
           03  BM-MEMO-LAST-LINE-SSNO   PIC 9(9).
           03  BM-MEMO-LAST-LINE-DATE   PIC 9(8).
           03  BM-MEMO-LAST-LINE-SEQ    PIC 9(3).

       01  DM-MEMO-HOLD-KEY.
           03  DM-MEMO-HOLD-BRNO   PIC 9(4).
           03  DM-MEMO-HOLD-DLNUM  PIC 9(6).
           03  DM-MEMO-HOLD-DATE   PIC 9(8).
           03  DM-MEMO-HOLD-SEQ    PIC 9(3).

       01  DM-MEMO-LINE1-KEY.
           03  DM-MEMO-LINE1-BRNO   PIC 9(4).
           03  DM-MEMO-LINE1-DLNUM  PIC 9(6).
           03  DM-MEMO-LINE1-DATE   PIC 9(8).
           03  DM-MEMO-LINE1-SEQ    PIC 9(3).

       01  DM-MEMO-LAST-LINE-KEY.
           03  DM-MEMO-LAST-LINE-BRNO   PIC 9(4).
           03  DM-MEMO-LAST-LINE-DLNO   PIC 9(4)V99.
           03  DM-MEMO-LAST-LINE-DATE   PIC 9(8).
           03  DM-MEMO-LAST-LINE-SEQ    PIC 9(3).

       01  VM-MEMO-HOLD-KEY.
           03  VM-MEMO-HOLD-NUMBER PIC 9(6).
           03  VM-MEMO-HOLD-DATE   PIC 9(8).
           03  VM-MEMO-HOLD-SEQ    PIC 9(3).

       01  VM-MEMO-LINE1-KEY.
           03  VM-MEMO-LINE1-SSNO   PIC 9(6).
           03  VM-MEMO-LINE1-DATE   PIC 9(8).
           03  VM-MEMO-LINE1-SEQ    PIC 9(3).

       01  VM-MEMO-LAST-LINE-KEY.
           03  VM-MEMO-LAST-LINE-NUMBER PIC 9(6).
           03  VM-MEMO-LAST-LINE-DATE   PIC 9(8).
           03  VM-MEMO-LAST-LINE-SEQ    PIC 9(3).

       01  MEMO-LIST.
           03  FILLER          PIC X(32)    VALUE
               "L01 L02 L03 L04 L05 L06 L07 L08.".

       01  MEMO-BUF              VALUE SPACES.
           03  MEMO-TAB          OCCURS 8.
               05  MEMO-DATE     PIC 99/99/99.
               05  MEMO-DATEX REDEFINES MEMO-DATE PIC X(8).
               05  FILLER        PIC X(3).
               05  MEMO-SEQ      PIC ZZZ.
               05  FILLER        PIC X(2).
               05  MEMO-MEMO     PIC X(60).
               05  MEMO-MEMOX REDEFINES MEMO-MEMO.
                   07  MEMO-PH-DESC PIC X(44).
                   07  MEMO-PH-NO   PIC 999/999/9999.
                   07  FILLER       PIC X.
                   07  MEMO-PH-EXT  PIC ZZZ.


       01  MEMO-ENTRY-LIST.
           03  FILLER     PIC X(27) VALUE
           "MEMODATE DUM1 MEMOSEQ DUM2 ".
           03  FILLER     PIC X(26) VALUE
           "MEMODESC DUM3 ACCEPT DUM4.".

       01  SAVE-BEG-LM-SEQ                 PIC 9(03)       VALUE ZEROES.
       01  SAVE-END-LM-SEQ                 PIC 9(03)       VALUE ZEROES.

       01  SAVE-BEG-NM-SEQ                 PIC 9(03)       VALUE ZEROES.
       01  SAVE-END-NM-SEQ                 PIC 9(03)       VALUE ZEROES.

       01  WS-MEMO-DATE.
           03  WS-MEMO-DATE-MDY            PIC 99/99/99.
           03  WS-MEMO-DATEX REDEFINES WS-MEMO-DATE-MDY
                                           PIC X(8).

       01  WS-SEQ                          PIC ZZZ         VALUE ZEROES.

       01  TEST-USER-PROFILE     PIC X.
       01  REG-MEMO.
           03  REG-MEMO-USERID   PIC X(09).
           03  REG-REST-OF-MEMO  PIC X(51).

       01  FMETRO-MEMO                 PIC X(60)     VALUE SPACES.
       01  FILLER                      REDEFINES FMETRO-MEMO.
           03  FMETRO-MEMO-ENTRY       PIC X(42).
           03  FILLER                  PIC X(01).
           03  FMETRO-MEMO-TIME        PIC 99/99/99.
           03  FILLER                  PIC X(01).
           03  FMETRO-MEMO-USERID      PIC X(08).
