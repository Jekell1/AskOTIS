      * COPYMEMBER: LIBSP/SP01CP_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - COLLECTOR POOL FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  CP1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  CP1-CURSOR-STAT-GOOD              VALUE "00".
           88  CP1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  CP1-RETURN-CODE           PIC S9(4)   COMP-5.

       01  QCP1-WHERE-BEG-KEY.
           03  QCP1-WBEG-BRNO             PIC 9(04).
           03  QCP1-WBEG-ID               PIC X(3).

       01  QCP1-WHERE-END-KEY.
           03  QCP1-WEND-BRNO             PIC 9(04).
           03  QCP1-WEND-ID               PIC X(3).
 
       01  QCP-REC.
           03  QCP-BRNO              PIC 9(4).
           03  QCP-ID                PIC X(3).
           03  QCP-LTOUCH-DATE       PIC X(10).
           03  QCP-DESC              PIC X(20).
           03  QCP-COL-ID-1          PIC X(3).
           03  QCP-COL-ID-2          PIC X(3).
           03  QCP-COL-ID-3          PIC X(3).
           03  QCP-COL-ID-4          PIC X(3).
           03  QCP-COL-ID-5          PIC X(3).
           03  QCP-COL-ID-6          PIC X(3).
           03  QCP-COL-ID-7          PIC X(3).
           03  QCP-COL-ID-8          PIC X(3).
           03  QCP-COL-ID-9          PIC X(3).
           03  QCP-COL-ID-10         PIC X(3).
           03  QCP-COL-ID-11         PIC X(3).
           03  QCP-COL-ID-12         PIC X(3).
           03  QCP-COL-ID-13         PIC X(3).
           03  QCP-COL-ID-14         PIC X(3).
           03  QCP-COL-ID-15         PIC X(3).
           03  QCP-COL-ID-16         PIC X(3).
           03  QCP-COL-ID-17         PIC X(3).
           03  QCP-COL-ID-18         PIC X(3).
           03  QCP-COL-ID-19         PIC X(3).
           03  QCP-COL-ID-20         PIC X(3).
           03  QCP-COL-ID-21         PIC X(3).
           03  QCP-COL-ID-22         PIC X(3).
           03  QCP-COL-ID-23         PIC X(3).
           03  QCP-COL-ID-24         PIC X(3).
           03  QCP-COL-ID-25         PIC X(3).
           03  QCP-COL-ID-26         PIC X(3).
           03  QCP-COL-ID-27         PIC X(3).
           03  QCP-COL-ID-28         PIC X(3).
           03  QCP-COL-ID-29         PIC X(3).
           03  QCP-COL-ID-30         PIC X(3).
           03  QCP-COL-ID-31         PIC X(3).
           03  QCP-COL-ID-32         PIC X(3).
           03  QCP-COL-ID-33         PIC X(3).
           03  QCP-COL-ID-34         PIC X(3).
           03  QCP-COL-ID-35         PIC X(3).
           03  QCP-COL-ID-36         PIC X(3).
           03  QCP-COL-ID-37         PIC X(3).
           03  QCP-COL-ID-38         PIC X(3).
           03  QCP-COL-ID-39         PIC X(3).
           03  QCP-COL-ID-40         PIC X(3).
           03  QCP-COL-ID-41         PIC X(3).
           03  QCP-COL-ID-42         PIC X(3).
           03  QCP-COL-ID-43         PIC X(3).
           03  QCP-COL-ID-44         PIC X(3).
           03  QCP-COL-ID-45         PIC X(3).
           03  QCP-COL-ID-46         PIC X(3).
           03  QCP-COL-ID-47         PIC X(3).
           03  QCP-COL-ID-48         PIC X(3).
           03  QCP-COL-ID-49         PIC X(3).
           03  QCP-COL-ID-50         PIC X(3).
           03  QCP-COL-ID-51         PIC X(3).
           03  QCP-COL-ID-52         PIC X(3).
           03  QCP-COL-ID-53         PIC X(3).
           03  QCP-COL-ID-54         PIC X(3).
           03  QCP-COL-ID-55         PIC X(3).
           03  QCP-COL-ID-56         PIC X(3).
           03  QCP-COL-ID-57         PIC X(3).
           03  QCP-COL-ID-58         PIC X(3).
           03  QCP-COL-ID-59         PIC X(3).
           03  QCP-COL-ID-60         PIC X(3).
           03  QCP-COL-ID-61         PIC X(3).
           03  QCP-COL-ID-62         PIC X(3).
           03  QCP-COL-ID-63         PIC X(3).
           03  QCP-COL-ID-64         PIC X(3).
           03  QCP-COL-ID-65         PIC X(3).
           03  QCP-COL-ID-66         PIC X(3).
           03  QCP-COL-ID-67         PIC X(3).
           03  QCP-COL-ID-68         PIC X(3).
           03  QCP-COL-ID-69         PIC X(3).
           03  QCP-COL-ID-70         PIC X(3).
           03  QCP-COL-ID-71         PIC X(3).
           03  QCP-COL-ID-72         PIC X(3).
           03  QCP-COL-ID-73         PIC X(3).
           03  QCP-COL-ID-74         PIC X(3).
           03  QCP-COL-ID-75         PIC X(3).
           03  QCP-COL-ID-76         PIC X(3).
           03  QCP-COL-ID-77         PIC X(3).
           03  QCP-COL-ID-78         PIC X(3).
           03  QCP-COL-ID-79         PIC X(3).
           03  QCP-COL-ID-80         PIC X(3).
           03  QCP-ALPHA-FLAG        PIC X.
           03  QCP-TOTAL-ACCOUNTS    PIC 9(7).
           03  QCP-COLLECTOR-COUNT   PIC 9(3).
           03  QCP-LAST-SUBSCRIPT    PIC 9(3).

       EXEC SQL END DECLARE SECTION END-EXEC.
