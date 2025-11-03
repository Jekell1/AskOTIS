      * COPYMEMBER: LIBGB/ARRAYGIW
      ******************************************************************
      *
      * DESCRIPTION: ARRAY FOR GL INTERFACE PROCESSING (GIFILE)
      *
      *              USED TO LOAD RECORDS (GI-REC) FROM GIFILE SQL TABLE
      *              FOR BRANCH SPECIFIC ROWS.
      *
      * NEEDED     : LIBLP/LP01GI.CPY   (NEEDS 78 GI-SIZE)
      *
      *=================================================================
      * REV:
      * JKC 2023-1201 ***NEW***
      *
      ******************************************************************
       77  ARRAYGI-ELE                 PIC 9(3)      VALUE ZEROES.
       77  ARRAYGI-END                 PIC 9(3)      VALUE ZEROES.
       78  ARRAYGI-MAX                               VALUE 250.

       01  ARRAYGI-GI1-KEY.
           03  ARRAYGI-GI-BRANCH       PIC 9(4).
           03  ARRAYGI-GI-CLASS        PIC 9(3).

       01  ARRAYGI-FILE.
           03  ARRAYGI-REC             PIC X(GI-SIZE)
                                           OCCURS ARRAYGI-MAX.

       01  ARRAYGI-REC-READ            PIC X(GI-SIZE).

