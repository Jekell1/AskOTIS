      * COPYMEMBER: LIBGB/GB01GB_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - GLOBAL FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  GB-CURSOR-STAT               PIC X(2)  VALUE SPACES.
           88  GB-CURSOR-STAT-GOOD                VALUE "00".
           88  GB-CURSOR-STAT-BAD                 VALUE "  " THRU "//"
                                                        "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  GB1-RETURN-CODE               PIC S9(4)   COMP-5.

       01  QGB-WHERE-BEG-KEY.
           03  QGB-WBEG-BRNO             PIC 9(04).
           03  QGB-WBEG-NO               PIC 9.
 
       01  QGB-WHERE-END-KEY.
           03  QGB-WEND-BRNO             PIC 9(04).
           03  QGB-WEND-NO               PIC 9.
 
       01  QGB-REC.
           03  QGB-BRNO                      PIC 9(4).
           03  QGB-NO                        PIC 9.
           03  QGB-LTOUCH-DATE               PIC X(10).
           03  QGB-COMPANY-NAME              PIC X(40).
           03  QGB-ADDR-1                    PIC X(25).
           03  QGB-ADDR-2                    PIC X(25).
           03  QGB-CITY                      PIC X(15).
           03  QGB-STATE                     PIC XX.
           03  QGB-ZIP                       PIC X(10).
           03  QGB-PHONE                     PIC 9(10).
           03  QGB-NEXT-SF                   PIC 9(8).
           03  QGB-NEXT-SMALL                PIC 9(8).
           03  QGB-NEXT-OTHER                PIC 9(8).
           03  QGB-HOLIDAY-1                 PIC 9(4).
           03  QGB-HOLIDAY-2                 PIC 9(4).
           03  QGB-HOLIDAY-3                 PIC 9(4).
           03  QGB-HOLIDAY-4                 PIC 9(4).
           03  QGB-HOLIDAY-5                 PIC 9(4).
           03  QGB-HOLIDAY-6                 PIC 9(4).
           03  QGB-HOLIDAY-7                 PIC 9(4).
           03  QGB-HOLIDAY-8                 PIC 9(4).
           03  QGB-HOLIDAY-9                 PIC 9(4).
           03  QGB-HOLIDAY-10                PIC 9(4).
           03  QGB-HOLIDAY-11                PIC 9(4).
           03  QGB-HOLIDAY-12                PIC 9(4).
           03  QGB-HOLIDAY-13                PIC 9(4).
           03  QGB-HOLIDAY-14                PIC 9(4).
           03  QGB-HOLIDAY-15                PIC 9(4).
           03  QGB-HOLIDAY-16                PIC 9(4).
           03  QGB-RPT-LINES                 PIC 99.
           03  QGB-CHECK-PROG                PIC XX.
           03  QGB-OTHER-BRANCH-POST         PIC X.
           03  QGB-LAST-ACCRUE               PIC 9(4).
           03  QGB-AUTO-DEPOSIT              PIC X.
           03  QGB-UUCP-HOME                 PIC X(8).
           03  QGB-EOM-TAG-CCYYMM            PIC 9(6).
           03  QGB-EOM-UPDATE-DATE           PIC X(10).
           03  QGB-EOM-UPDATE-TIME           PIC 9(6).
           03  QGB-EOM-UPDATE-USERID         PIC X(10).
           03  QGB-FISCAL-EOY-TAG-CCYYMM     PIC 9(6).
           03  QGB-EOYF-UPDATE-DATE          PIC X(10).
           03  QGB-EOYF-UPDATE-TIME          PIC 9(6).
           03  QGB-EOYF-UPDATE-USERID        PIC X(10).
           03  QGB-CALENDAR-EOY-TAG-CCYYMM   PIC 9(6).
           03  QGB-EOYC-UPDATE-DATE          PIC X(10).
           03  QGB-EOYC-UPDATE-TIME          PIC 9(6).
           03  QGB-EOYC-UPDATE-USERID        PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
