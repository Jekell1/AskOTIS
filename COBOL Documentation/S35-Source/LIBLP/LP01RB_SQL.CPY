      * COPYMEMBER: LIBLP/LP01RB_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - REBATE/CANCELLATION TABLE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  RB1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  RB1-CURSOR-STAT-GOOD              VALUE "00".
           88  RB1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  RB1-RETURN-CODE              PIC S9(4)   COMP-5.

       01  QRB1-WHERE-BEG-KEY.
           03  QRB1-WBEG-TBL-NO         PIC 99.
           03  QRB1-WBEG-TERM           PIC 9(3).

       01  QRB1-WHERE-END-KEY.
           03  QRB1-WEND-TBL-NO         PIC 99.
           03  QRB1-WEND-TERM           PIC 9(3).

       01  QRB-REC.
           03  QRB-TBL-NO    PIC 99.
           03  QRB-TERM      PIC 9(3).
           03  QRB-MON-DAY-CD    PIC X.
           03  QRB-AMT-1       PIC 9(4).
           03  QRB-RATE-1      PIC 99V99.
           03  QRB-AMT-2       PIC 9(4).
           03  QRB-RATE-2      PIC 99V99.
           03  QRB-AMT-3       PIC 9(4).
           03  QRB-RATE-3      PIC 99V99.
           03  QRB-AMT-4       PIC 9(4).
           03  QRB-RATE-4      PIC 99V99.
           03  QRB-AMT-5       PIC 9(4).
           03  QRB-RATE-5      PIC 99V99.
           03  QRB-AMT-6       PIC 9(4).
           03  QRB-RATE-6      PIC 99V99.
           03  QRB-AMT-7       PIC 9(4).
           03  QRB-RATE-7      PIC 99V99.
           03  QRB-AMT-8       PIC 9(4).
           03  QRB-RATE-8      PIC 99V99.
           03  QRB-AMT-9       PIC 9(4).
           03  QRB-RATE-9      PIC 99V99.
           03  QRB-AMT-10      PIC 9(4).
           03  QRB-RATE-10     PIC 99V99.
           03  QRB-AMT-11       PIC 9(4).
           03  QRB-RATE-11      PIC 99V99.
           03  QRB-AMT-12       PIC 9(4).
           03  QRB-RATE-12      PIC 99V99.
           03  QRB-AMT-13       PIC 9(4).
           03  QRB-RATE-13      PIC 99V99.
           03  QRB-AMT-14       PIC 9(4).
           03  QRB-RATE-14      PIC 99V99.
           03  QRB-AMT-15       PIC 9(4).
           03  QRB-RATE-15      PIC 99V99.
           03  QRB-AMT-16       PIC 9(4).
           03  QRB-RATE-16      PIC 99V99.
           03  QRB-AMT-17       PIC 9(4).
           03  QRB-RATE-17      PIC 99V99.
           03  QRB-AMT-18       PIC 9(4).
           03  QRB-RATE-18      PIC 99V99.
           03  QRB-AMT-19       PIC 9(4).
           03  QRB-RATE-19      PIC 99V99.
           03  QRB-AMT-20       PIC 9(4).
           03  QRB-RATE-20      PIC 99V99.
           03  QRB-AMT-21       PIC 9(4).
           03  QRB-RATE-21      PIC 99V99.
           03  QRB-AMT-22       PIC 9(4).
           03  QRB-RATE-22      PIC 99V99.
           03  QRB-AMT-23       PIC 9(4).
           03  QRB-RATE-23      PIC 99V99.
           03  QRB-AMT-24       PIC 9(4).
           03  QRB-RATE-24      PIC 99V99.
           03  QRB-AMT-25       PIC 9(4).
           03  QRB-RATE-25      PIC 99V99.
           03  QRB-AMT-26       PIC 9(4).
           03  QRB-RATE-26      PIC 99V99.
           03  QRB-AMT-27       PIC 9(4).
           03  QRB-RATE-27      PIC 99V99.
           03  QRB-AMT-28       PIC 9(4).
           03  QRB-RATE-28      PIC 99V99.
           03  QRB-AMT-29       PIC 9(4).
           03  QRB-RATE-29      PIC 99V99.
           03  QRB-AMT-30       PIC 9(4).
           03  QRB-RATE-30      PIC 99V99.
           03  QRB-AMT-31       PIC 9(4).
           03  QRB-RATE-31      PIC 99V99.
           03  QRB-AMT-32       PIC 9(4).
           03  QRB-RATE-32      PIC 99V99.
           03  QRB-AMT-33       PIC 9(4).
           03  QRB-RATE-33      PIC 99V99.
           03  QRB-AMT-34       PIC 9(4).
           03  QRB-RATE-34      PIC 99V99.
           03  QRB-AMT-35       PIC 9(4).
           03  QRB-RATE-35      PIC 99V99.
           03  QRB-AMT-36       PIC 9(4).
           03  QRB-RATE-36      PIC 99V99.
           03  QRB-AMT-37       PIC 9(4).
           03  QRB-RATE-37      PIC 99V99.
           03  QRB-AMT-38       PIC 9(4).
           03  QRB-RATE-38      PIC 99V99.
           03  QRB-AMT-39       PIC 9(4).
           03  QRB-RATE-39      PIC 99V99.
           03  QRB-AMT-40       PIC 9(4).
           03  QRB-RATE-40      PIC 99V99.
           03  QRB-AMT-41       PIC 9(4).
           03  QRB-RATE-41      PIC 99V99.
           03  QRB-AMT-42       PIC 9(4).
           03  QRB-RATE-42      PIC 99V99.
           03  QRB-AMT-43       PIC 9(4).
           03  QRB-RATE-43      PIC 99V99.
           03  QRB-AMT-44       PIC 9(4).
           03  QRB-RATE-44      PIC 99V99.
           03  QRB-AMT-45       PIC 9(4).
           03  QRB-RATE-45      PIC 99V99.
           03  QRB-AMT-46       PIC 9(4).
           03  QRB-RATE-46      PIC 99V99.
           03  QRB-AMT-47       PIC 9(4).
           03  QRB-RATE-47      PIC 99V99.
           03  QRB-AMT-48       PIC 9(4).
           03  QRB-RATE-48      PIC 99V99.
           03  QRB-AMT-49       PIC 9(4).
           03  QRB-RATE-49      PIC 99V99.
           03  QRB-AMT-50       PIC 9(4).
           03  QRB-RATE-50      PIC 99V99.
           03  QRB-AMT-51       PIC 9(4).
           03  QRB-RATE-51      PIC 99V99.
           03  QRB-AMT-52       PIC 9(4).
           03  QRB-RATE-52      PIC 99V99.
           03  QRB-AMT-53       PIC 9(4).
           03  QRB-RATE-53      PIC 99V99.
           03  QRB-AMT-54       PIC 9(4).
           03  QRB-RATE-54      PIC 99V99.
           03  QRB-AMT-55       PIC 9(4).
           03  QRB-RATE-55      PIC 99V99.
           03  QRB-AMT-56       PIC 9(4).
           03  QRB-RATE-56      PIC 99V99.
           03  QRB-AMT-57       PIC 9(4).
           03  QRB-RATE-57      PIC 99V99.
           03  QRB-AMT-58       PIC 9(4).
           03  QRB-RATE-58      PIC 99V99.
           03  QRB-AMT-59       PIC 9(4).
           03  QRB-RATE-59      PIC 99V99.
           03  QRB-AMT-60       PIC 9(4).
           03  QRB-RATE-60      PIC 99V99.
           03  QRB-AMT-61       PIC 9(4).
           03  QRB-RATE-61      PIC 99V99.
           03  QRB-AMT-62       PIC 9(4).
           03  QRB-RATE-62      PIC 99V99.
           03  QRB-AMT-63       PIC 9(4).
           03  QRB-RATE-63      PIC 99V99.
           03  QRB-AMT-64       PIC 9(4).
           03  QRB-RATE-64      PIC 99V99.
           03  QRB-AMT-65       PIC 9(4).
           03  QRB-RATE-65      PIC 99V99.
           03  QRB-AMT-66       PIC 9(4).
           03  QRB-RATE-66      PIC 99V99.
           03  QRB-AMT-67       PIC 9(4).
           03  QRB-RATE-67      PIC 99V99.
           03  QRB-AMT-68       PIC 9(4).
           03  QRB-RATE-68      PIC 99V99.
           03  QRB-AMT-69       PIC 9(4).
           03  QRB-RATE-69      PIC 99V99.
           03  QRB-AMT-70       PIC 9(4).
           03  QRB-RATE-70      PIC 99V99.
           03  QRB-AMT-71       PIC 9(4).
           03  QRB-RATE-71      PIC 99V99.
           03  QRB-AMT-72       PIC 9(4).
           03  QRB-RATE-72      PIC 99V99.
           03  QRB-AMT-73       PIC 9(4).
           03  QRB-RATE-73      PIC 99V99.
           03  QRB-AMT-74       PIC 9(4).
           03  QRB-RATE-74      PIC 99V99.
           03  QRB-AMT-75       PIC 9(4).
           03  QRB-RATE-75      PIC 99V99.
           03  QRB-LTOUCH-DATE   PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
