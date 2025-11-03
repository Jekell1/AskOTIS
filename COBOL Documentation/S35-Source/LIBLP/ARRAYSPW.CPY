      * COPYMEMBER: LIBLP/ARRAYSPW
      ******************************************************************
      *
      * DESCRIPTION: ARRAY FOR STATE PARAMETER RECORDS
      *
      *              USED TO LOAD *ALL* SP-REC FROM THE SPFILE SQL TABLE
      *              INTO AN INTERNAL ARRAY FOR IMPROVING PERFORMANCE.
      *
      * NEEDED     : LIBLP/LP01SP.CPY   (NEEDS 78 SP-SIZE)
      *
      *=================================================================
      * REV:
      * JKC 2023-0609 ***NEW***
      *
      ******************************************************************
       78  ARRAYSP-MAX                               VALUE 1500.
       77  ARRAYSP-ELE                 PIC 9(4)      VALUE ZEROES.
       77  ARRAYSP-END                 PIC 9(4)      VALUE ZEROES.

       01  ARRAYSP-KEY.
           05  ARRAYSP-ORGST           PIC X(2)      VALUE SPACES.
           05  ARRAYSP-SPRCLASS        PIC 9(3)      VALUE ZEROES.
           05  ARRAYSP-SUBCLASS        PIC 9(2)      VALUE ZEROES.
           05  ARRAYSP-LAWCODE         PIC 9(2)      VALUE ZEROES.

       01  ARRAYSP-FILE.
           03  ARRAYSP-REC             PIC X(SP-SIZE)
                                           OCCURS ARRAYSP-MAX.

       01  ARRAYSP-REC-READ            PIC X(SP-SIZE).

