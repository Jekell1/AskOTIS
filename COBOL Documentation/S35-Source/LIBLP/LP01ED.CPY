      * COPYMEMBER: LIBLP/LP01ED
      **************************************************
      *      E.O.D. REPORT WORK FILE                   *
      *      (EACH REPORT TO DEFINE ED-REC)            *
      * REV:
      * BLV 091097 PUT FIELDS IN THIS COPY MEMBER
      **************************************************
       01  ED-REC.
           03  ED-KEY.
               05  ED-NO              PIC 9(11).
           03  ED-ID                  PIC X.
           03  ED-AMT  OCCURS 24      PIC S9(9)V99 COMP-3.
           03  FILLER                 PIC X(51).
           03  ED-UPDATED             PIC X.
