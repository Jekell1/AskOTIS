      * COPYMEMBER: LIBLP/LP01IR_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - INSURANCE RATE FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  IR-CURSOR-STAT               PIC X(2) VALUE SPACES.
           88  IR-CURSOR-STAT-GOOD               VALUE "00".
           88  IR-CURSOR-STAT-BAD                VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  IR1-RETURN-CODE            PIC S9(4)   COMP-5.

       01  QIR-WHERE-BEG-KEY.
           03  QIR-WBEG-KEY            PIC 9(3).
 
       01  QIR-WHERE-END-KEY.
           03  QIR-WEND-KEY            PIC 9(3).
 
       01  QIR-REC.
           03  QIR-KEY                PIC 999.
           03  QIR-LTOUCH-DATE        PIC X(10).
           03  QIR-PER-FG             PIC X.
           03  QIR-INSRATE-1          PIC 99V999.
           03  QIR-INSRATE-2          PIC 99V999.
           03  QIR-INSRATE-3          PIC 99V999.
           03  QIR-INSRATE-4          PIC 99V999.
           03  QIR-INSRATE-5          PIC 99V999.
           03  QIR-INSRATE-6          PIC 99V999.
           03  QIR-INSRATE-7          PIC 99V999.
           03  QIR-INSRATE-8          PIC 99V999.
           03  QIR-INSRATE-9          PIC 99V999.
           03  QIR-INSRATE-10         PIC 99V999.
           03  QIR-INSRATE-11          PIC 99V999.
           03  QIR-INSRATE-12          PIC 99V999.
           03  QIR-INSRATE-13          PIC 99V999.
           03  QIR-INSRATE-14          PIC 99V999.
           03  QIR-INSRATE-15          PIC 99V999.
           03  QIR-INSRATE-16          PIC 99V999.
           03  QIR-INSRATE-17          PIC 99V999.
           03  QIR-INSRATE-18          PIC 99V999.
           03  QIR-INSRATE-19          PIC 99V999.
           03  QIR-INSRATE-20         PIC 99V999.
           03  QIR-INSRATE-21          PIC 99V999.
           03  QIR-INSRATE-22          PIC 99V999.
           03  QIR-INSRATE-23          PIC 99V999.
           03  QIR-INSRATE-24          PIC 99V999.
           03  QIR-INSRATE-25          PIC 99V999.
           03  QIR-INSRATE-26          PIC 99V999.
           03  QIR-INSRATE-27          PIC 99V999.
           03  QIR-INSRATE-28          PIC 99V999.
           03  QIR-INSRATE-29          PIC 99V999.
           03  QIR-INSRATE-30         PIC 99V999.
           03  QIR-INSRATE-31          PIC 99V999.
           03  QIR-INSRATE-32          PIC 99V999.
           03  QIR-INSRATE-33          PIC 99V999.
           03  QIR-INSRATE-34          PIC 99V999.
           03  QIR-INSRATE-35          PIC 99V999.
           03  QIR-INSRATE-36          PIC 99V999.
           03  QIR-INSRATE-37          PIC 99V999.
           03  QIR-INSRATE-38          PIC 99V999.
           03  QIR-INSRATE-39          PIC 99V999.
           03  QIR-INSRATE-40         PIC 99V999.
           03  QIR-INSRATE-41          PIC 99V999.
           03  QIR-INSRATE-42          PIC 99V999.
           03  QIR-INSRATE-43          PIC 99V999.
           03  QIR-INSRATE-44          PIC 99V999.
           03  QIR-INSRATE-45          PIC 99V999.
           03  QIR-INSRATE-46          PIC 99V999.
           03  QIR-INSRATE-47          PIC 99V999.
           03  QIR-INSRATE-48          PIC 99V999.
           03  QIR-INSRATE-49          PIC 99V999.
           03  QIR-INSRATE-50         PIC 99V999.
           03  QIR-INSRATE-51          PIC 99V999.
           03  QIR-INSRATE-52          PIC 99V999.
           03  QIR-INSRATE-53          PIC 99V999.
           03  QIR-INSRATE-54          PIC 99V999.
           03  QIR-INSRATE-55          PIC 99V999.
           03  QIR-INSRATE-56          PIC 99V999.
           03  QIR-INSRATE-57          PIC 99V999.
           03  QIR-INSRATE-58          PIC 99V999.
           03  QIR-INSRATE-59          PIC 99V999.
           03  QIR-INSRATE-60         PIC 99V999.
           03  QIR-INSRATE-61          PIC 99V999.
           03  QIR-INSRATE-62          PIC 99V999.
           03  QIR-INSRATE-63          PIC 99V999.
           03  QIR-INSRATE-64          PIC 99V999.
           03  QIR-INSRATE-65          PIC 99V999.
           03  QIR-INSRATE-66          PIC 99V999.
           03  QIR-INSRATE-67          PIC 99V999.
           03  QIR-INSRATE-68          PIC 99V999.
           03  QIR-INSRATE-69          PIC 99V999.
           03  QIR-INSRATE-70         PIC 99V999.
           03  QIR-INSRATE-71          PIC 99V999.
           03  QIR-INSRATE-72          PIC 99V999.
           03  QIR-INSRATE-73          PIC 99V999.
           03  QIR-INSRATE-74          PIC 99V999.
           03  QIR-INSRATE-75          PIC 99V999.
           03  QIR-INSRATE-76          PIC 99V999.
           03  QIR-INSRATE-77          PIC 99V999.
           03  QIR-INSRATE-78          PIC 99V999.
           03  QIR-INSRATE-79          PIC 99V999.
           03  QIR-INSRATE-80         PIC 99V999.
           03  QIR-INSRATE-81          PIC 99V999.
           03  QIR-INSRATE-82          PIC 99V999.
           03  QIR-INSRATE-83          PIC 99V999.
           03  QIR-INSRATE-84          PIC 99V999.
           03  QIR-INSRATE-85          PIC 99V999.
           03  QIR-INSRATE-86          PIC 99V999.
           03  QIR-INSRATE-87          PIC 99V999.
           03  QIR-INSRATE-88          PIC 99V999.
           03  QIR-INSRATE-89          PIC 99V999.
           03  QIR-INSRATE-90         PIC 99V999.
           03  QIR-INSRATE-91          PIC 99V999.
           03  QIR-INSRATE-92          PIC 99V999.
           03  QIR-INSRATE-93          PIC 99V999.
           03  QIR-INSRATE-94          PIC 99V999.
           03  QIR-INSRATE-95          PIC 99V999.
           03  QIR-INSRATE-96          PIC 99V999.
           03  QIR-INSRATE-97          PIC 99V999.
           03  QIR-INSRATE-98          PIC 99V999.
           03  QIR-INSRATE-99          PIC 99V999.
           03  QIR-INSRATE-100         PIC 99V999.
           03  QIR-INSRATE-101         PIC 99V999.
           03  QIR-INSRATE-102         PIC 99V999.
           03  QIR-INSRATE-103         PIC 99V999.
           03  QIR-INSRATE-104         PIC 99V999.
           03  QIR-INSRATE-105         PIC 99V999.
           03  QIR-INSRATE-106         PIC 99V999.
           03  QIR-INSRATE-107         PIC 99V999.
           03  QIR-INSRATE-108         PIC 99V999.
           03  QIR-INSRATE-109         PIC 99V999.
           03  QIR-INSRATE-110         PIC 99V999.
           03  QIR-INSRATE-111         PIC 99V999.
           03  QIR-INSRATE-112         PIC 99V999.
           03  QIR-INSRATE-113         PIC 99V999.
           03  QIR-INSRATE-114         PIC 99V999.
           03  QIR-INSRATE-115         PIC 99V999.
           03  QIR-INSRATE-116         PIC 99V999.
           03  QIR-INSRATE-117         PIC 99V999.
           03  QIR-INSRATE-118         PIC 99V999.
           03  QIR-INSRATE-119         PIC 99V999.
           03  QIR-INSRATE-120         PIC 99V999.
           03  QIR-DESC               PIC X(40).

       EXEC SQL END DECLARE SECTION END-EXEC.
