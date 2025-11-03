      * COPYMEMBER: LIBLP/LP01ES_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN ESCROW FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  ES1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  ES1-CURSOR-STAT-GOOD              VALUE "00".
           88  ES1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  ES1-CURSOR-GR-STAT           PIC X(2) VALUE SPACES.
           88  ES1-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  ES1-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  ES2-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  ES2-CURSOR-STAT-GOOD              VALUE "00".
           88  ES2-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  ES2-CURSOR-GR-STAT           PIC X(2) VALUE SPACES.
           88  ES2-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  ES2-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  ES1-RETURN-CODE                 PIC S9(4)   COMP-5.

       01  QES1-WHERE-BEG-KEY.
           03  QES1-WBEG-BRNO              PIC 9(4).
           03  QES1-WBEG-ACCTNO            PIC 9(8).
 
       01  QES1-WHERE-END-KEY.
           03  QES1-WEND-BRNO              PIC 9(4).
           03  QES1-WEND-ACCTNO            PIC 9(8).
 
       01  QES-REC.
           03  QES-BRNO                      PIC 9(4).
           03  QES-ACCTNO                    PIC 9(8).
           03  QES-LTOUCH-DATE               PIC X(10).
           03  QES-SCHD-EFF-DATE-1           PIC X(10).
           03  QES-SCHD-1STPYDATE-1          PIC X(10).
           03  QES-SCHD-ESCROW-1             PIC S9(7)V99.
           03  QES-SCHD-TERM-1               PIC 9(3).
           03  QES-SCHD-COLLECTED-1          PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-2           PIC X(10).
           03  QES-SCHD-1STPYDATE-2          PIC X(10).
           03  QES-SCHD-ESCROW-2             PIC S9(7)V99.
           03  QES-SCHD-TERM-2               PIC 9(3).
           03  QES-SCHD-COLLECTED-2          PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-3           PIC X(10).
           03  QES-SCHD-1STPYDATE-3          PIC X(10).
           03  QES-SCHD-ESCROW-3             PIC S9(7)V99.
           03  QES-SCHD-TERM-3               PIC 9(3).
           03  QES-SCHD-COLLECTED-3          PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-4           PIC X(10).
           03  QES-SCHD-1STPYDATE-4          PIC X(10).
           03  QES-SCHD-ESCROW-4             PIC S9(7)V99.
           03  QES-SCHD-TERM-4               PIC 9(3).
           03  QES-SCHD-COLLECTED-4          PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-5           PIC X(10).
           03  QES-SCHD-1STPYDATE-5          PIC X(10).
           03  QES-SCHD-ESCROW-5             PIC S9(7)V99.
           03  QES-SCHD-TERM-5               PIC 9(3).
           03  QES-SCHD-COLLECTED-5          PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-6           PIC X(10).
           03  QES-SCHD-1STPYDATE-6          PIC X(10).
           03  QES-SCHD-ESCROW-6             PIC S9(7)V99.
           03  QES-SCHD-TERM-6               PIC 9(3).
           03  QES-SCHD-COLLECTED-6          PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-7           PIC X(10).
           03  QES-SCHD-1STPYDATE-7          PIC X(10).
           03  QES-SCHD-ESCROW-7             PIC S9(7)V99.
           03  QES-SCHD-TERM-7               PIC 9(3).
           03  QES-SCHD-COLLECTED-7          PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-8           PIC X(10).
           03  QES-SCHD-1STPYDATE-8          PIC X(10).
           03  QES-SCHD-ESCROW-8             PIC S9(7)V99.
           03  QES-SCHD-TERM-8               PIC 9(3).
           03  QES-SCHD-COLLECTED-8          PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-9           PIC X(10).
           03  QES-SCHD-1STPYDATE-9          PIC X(10).
           03  QES-SCHD-ESCROW-9             PIC S9(7)V99.
           03  QES-SCHD-TERM-9               PIC 9(3).
           03  QES-SCHD-COLLECTED-9          PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-10          PIC X(10).
           03  QES-SCHD-1STPYDATE-10         PIC X(10).
           03  QES-SCHD-ESCROW-10            PIC S9(7)V99.
           03  QES-SCHD-TERM-10              PIC 9(3).
           03  QES-SCHD-COLLECTED-10         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-11          PIC X(10).
           03  QES-SCHD-1STPYDATE-11         PIC X(10).
           03  QES-SCHD-ESCROW-11            PIC S9(7)V99.
           03  QES-SCHD-TERM-11              PIC 9(3).
           03  QES-SCHD-COLLECTED-11         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-12          PIC X(10).
           03  QES-SCHD-1STPYDATE-12         PIC X(10).
           03  QES-SCHD-ESCROW-12            PIC S9(7)V99.
           03  QES-SCHD-TERM-12              PIC 9(3).
           03  QES-SCHD-COLLECTED-12         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-13          PIC X(10).
           03  QES-SCHD-1STPYDATE-13         PIC X(10).
           03  QES-SCHD-ESCROW-13            PIC S9(7)V99.
           03  QES-SCHD-TERM-13              PIC 9(3).
           03  QES-SCHD-COLLECTED-13         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-14          PIC X(10).
           03  QES-SCHD-1STPYDATE-14         PIC X(10).
           03  QES-SCHD-ESCROW-14            PIC S9(7)V99.
           03  QES-SCHD-TERM-14              PIC 9(3).
           03  QES-SCHD-COLLECTED-14         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-15          PIC X(10).
           03  QES-SCHD-1STPYDATE-15         PIC X(10).
           03  QES-SCHD-ESCROW-15            PIC S9(7)V99.
           03  QES-SCHD-TERM-15              PIC 9(3).
           03  QES-SCHD-COLLECTED-15         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-16          PIC X(10).
           03  QES-SCHD-1STPYDATE-16         PIC X(10).
           03  QES-SCHD-ESCROW-16            PIC S9(7)V99.
           03  QES-SCHD-TERM-16              PIC 9(3).
           03  QES-SCHD-COLLECTED-16         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-17          PIC X(10).
           03  QES-SCHD-1STPYDATE-17         PIC X(10).
           03  QES-SCHD-ESCROW-17            PIC S9(7)V99.
           03  QES-SCHD-TERM-17              PIC 9(3).
           03  QES-SCHD-COLLECTED-17         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-18          PIC X(10).
           03  QES-SCHD-1STPYDATE-18         PIC X(10).
           03  QES-SCHD-ESCROW-18            PIC S9(7)V99.
           03  QES-SCHD-TERM-18              PIC 9(3).
           03  QES-SCHD-COLLECTED-18         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-19          PIC X(10).
           03  QES-SCHD-1STPYDATE-19         PIC X(10).
           03  QES-SCHD-ESCROW-19            PIC S9(7)V99.
           03  QES-SCHD-TERM-19              PIC 9(3).
           03  QES-SCHD-COLLECTED-19         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-20          PIC X(10).
           03  QES-SCHD-1STPYDATE-20         PIC X(10).
           03  QES-SCHD-ESCROW-20            PIC S9(7)V99.
           03  QES-SCHD-TERM-20              PIC 9(3).
           03  QES-SCHD-COLLECTED-20         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-21          PIC X(10).
           03  QES-SCHD-1STPYDATE-21         PIC X(10).
           03  QES-SCHD-ESCROW-21            PIC S9(7)V99.
           03  QES-SCHD-TERM-21              PIC 9(3).
           03  QES-SCHD-COLLECTED-21         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-22          PIC X(10).
           03  QES-SCHD-1STPYDATE-22         PIC X(10).
           03  QES-SCHD-ESCROW-22            PIC S9(7)V99.
           03  QES-SCHD-TERM-22              PIC 9(3).
           03  QES-SCHD-COLLECTED-22         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-23          PIC X(10).
           03  QES-SCHD-1STPYDATE-23         PIC X(10).
           03  QES-SCHD-ESCROW-23            PIC S9(7)V99.
           03  QES-SCHD-TERM-23              PIC 9(3).
           03  QES-SCHD-COLLECTED-23         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-24          PIC X(10).
           03  QES-SCHD-1STPYDATE-24         PIC X(10).
           03  QES-SCHD-ESCROW-24            PIC S9(7)V99.
           03  QES-SCHD-TERM-24              PIC 9(3).
           03  QES-SCHD-COLLECTED-24         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-25          PIC X(10).
           03  QES-SCHD-1STPYDATE-25         PIC X(10).
           03  QES-SCHD-ESCROW-25            PIC S9(7)V99.
           03  QES-SCHD-TERM-25              PIC 9(3).
           03  QES-SCHD-COLLECTED-25         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-26          PIC X(10).
           03  QES-SCHD-1STPYDATE-26         PIC X(10).
           03  QES-SCHD-ESCROW-26            PIC S9(7)V99.
           03  QES-SCHD-TERM-26              PIC 9(3).
           03  QES-SCHD-COLLECTED-26         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-27          PIC X(10).
           03  QES-SCHD-1STPYDATE-27         PIC X(10).
           03  QES-SCHD-ESCROW-27            PIC S9(7)V99.
           03  QES-SCHD-TERM-27              PIC 9(3).
           03  QES-SCHD-COLLECTED-27         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-28          PIC X(10).
           03  QES-SCHD-1STPYDATE-28         PIC X(10).
           03  QES-SCHD-ESCROW-28            PIC S9(7)V99.
           03  QES-SCHD-TERM-28              PIC 9(3).
           03  QES-SCHD-COLLECTED-28         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-29          PIC X(10).
           03  QES-SCHD-1STPYDATE-29         PIC X(10).
           03  QES-SCHD-ESCROW-29            PIC S9(7)V99.
           03  QES-SCHD-TERM-29              PIC 9(3).
           03  QES-SCHD-COLLECTED-29         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-30          PIC X(10).
           03  QES-SCHD-1STPYDATE-30         PIC X(10).
           03  QES-SCHD-ESCROW-30            PIC S9(7)V99.
           03  QES-SCHD-TERM-30              PIC 9(3).
           03  QES-SCHD-COLLECTED-30         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-31          PIC X(10).
           03  QES-SCHD-1STPYDATE-31         PIC X(10).
           03  QES-SCHD-ESCROW-31            PIC S9(7)V99.
           03  QES-SCHD-TERM-31              PIC 9(3).
           03  QES-SCHD-COLLECTED-31         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-32          PIC X(10).
           03  QES-SCHD-1STPYDATE-32         PIC X(10).
           03  QES-SCHD-ESCROW-32            PIC S9(7)V99.
           03  QES-SCHD-TERM-32              PIC 9(3).
           03  QES-SCHD-COLLECTED-32         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-33          PIC X(10).
           03  QES-SCHD-1STPYDATE-33         PIC X(10).
           03  QES-SCHD-ESCROW-33            PIC S9(7)V99.
           03  QES-SCHD-TERM-33              PIC 9(3).
           03  QES-SCHD-COLLECTED-33         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-34          PIC X(10).
           03  QES-SCHD-1STPYDATE-34         PIC X(10).
           03  QES-SCHD-ESCROW-34            PIC S9(7)V99.
           03  QES-SCHD-TERM-34              PIC 9(3).
           03  QES-SCHD-COLLECTED-34         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-35          PIC X(10).
           03  QES-SCHD-1STPYDATE-35         PIC X(10).
           03  QES-SCHD-ESCROW-35            PIC S9(7)V99.
           03  QES-SCHD-TERM-35              PIC 9(3).
           03  QES-SCHD-COLLECTED-35         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-36          PIC X(10).
           03  QES-SCHD-1STPYDATE-36         PIC X(10).
           03  QES-SCHD-ESCROW-36            PIC S9(7)V99.
           03  QES-SCHD-TERM-36              PIC 9(3).
           03  QES-SCHD-COLLECTED-36         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-37          PIC X(10).
           03  QES-SCHD-1STPYDATE-37         PIC X(10).
           03  QES-SCHD-ESCROW-37            PIC S9(7)V99.
           03  QES-SCHD-TERM-37              PIC 9(3).
           03  QES-SCHD-COLLECTED-37         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-38          PIC X(10).
           03  QES-SCHD-1STPYDATE-38         PIC X(10).
           03  QES-SCHD-ESCROW-38            PIC S9(7)V99.
           03  QES-SCHD-TERM-38              PIC 9(3).
           03  QES-SCHD-COLLECTED-38         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-39          PIC X(10).
           03  QES-SCHD-1STPYDATE-39         PIC X(10).
           03  QES-SCHD-ESCROW-39            PIC S9(7)V99.
           03  QES-SCHD-TERM-39              PIC 9(3).
           03  QES-SCHD-COLLECTED-39         PIC S9(7)V99.
           03  QES-SCHD-EFF-DATE-40          PIC X(10).
           03  QES-SCHD-1STPYDATE-40         PIC X(10).
           03  QES-SCHD-ESCROW-40            PIC S9(7)V99.
           03  QES-SCHD-TERM-40              PIC 9(3).
           03  QES-SCHD-COLLECTED-40         PIC S9(7)V99.
           03  QES-BALANCE                   PIC S9(7)V99.
           03  QES-PREPAID-1                 PIC S9(7)V99.
           03  QES-COLLECTED-1               PIC S9(7)V99.
           03  QES-DISBURSED-TAXES-1         PIC S9(7)V99.
           03  QES-DISBURSED-INS-1           PIC S9(7)V99.
           03  QES-DISBURSED-PMI-1           PIC S9(7)V99.
           03  QES-DISBURSED-OTHER-1         PIC S9(7)V99.
           03  QES-DISBURSED-REF-1           PIC S9(7)V99.
           03  QES-DISBURSED-CNT-TAXES-1     PIC S9(7)V99.
           03  QES-DISBURSED-MISC1-1         PIC S9(7)V99.
           03  QES-DISBURSED-MISC2-1         PIC S9(7)V99.
           03  QES-PREPAID-2                 PIC S9(7)V99.
           03  QES-COLLECTED-2               PIC S9(7)V99.
           03  QES-DISBURSED-TAXES-2         PIC S9(7)V99.
           03  QES-DISBURSED-INS-2           PIC S9(7)V99.
           03  QES-DISBURSED-PMI-2           PIC S9(7)V99.
           03  QES-DISBURSED-OTHER-2         PIC S9(7)V99.
           03  QES-DISBURSED-REF-2           PIC S9(7)V99.
           03  QES-DISBURSED-CNT-TAXES-2     PIC S9(7)V99.
           03  QES-DISBURSED-MISC1-2         PIC S9(7)V99.
           03  QES-DISBURSED-MISC2-2         PIC S9(7)V99.
           03  QES-PREPAID-3                 PIC S9(7)V99.
           03  QES-COLLECTED-3               PIC S9(7)V99.
           03  QES-DISBURSED-TAXES-3         PIC S9(7)V99.
           03  QES-DISBURSED-INS-3           PIC S9(7)V99.
           03  QES-DISBURSED-PMI-3           PIC S9(7)V99.
           03  QES-DISBURSED-OTHER-3         PIC S9(7)V99.
           03  QES-DISBURSED-REF-3           PIC S9(7)V99.
           03  QES-DISBURSED-CNT-TAXES-3     PIC S9(7)V99.
           03  QES-DISBURSED-MISC1-3         PIC S9(7)V99.
           03  QES-DISBURSED-MISC2-3         PIC S9(7)V99.
           03  QES-MISC-AMT-1                PIC S9(7)V99.
           03  QES-EXP-DATE-1                PIC X(10).
           03  QES-MISC-AMT-2                PIC S9(7)V99.
           03  QES-EXP-DATE-2                PIC X(10).
           03  QES-MISC-AMT-3                PIC S9(7)V99.
           03  QES-EXP-DATE-3                PIC X(10).
           03  QES-MISC-AMT-4                PIC S9(7)V99.
           03  QES-EXP-DATE-4                PIC X(10).
           03  QES-MISC-AMT-5                PIC S9(7)V99.
           03  QES-EXP-DATE-5                PIC X(10).
           03  QES-MISC-AMT-6                PIC S9(7)V99.
           03  QES-EXP-DATE-6                PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
