      * COPYMEMBER: LIBGB/ARRAYLCW
      ******************************************************************
      *
      * DESCRIPTION: ARRAY FOR LOAN CLASS PROCESSING (LCFILE)
      *
      *              USED TO LOAD *ALL* LFCILE LC_REQE BY LC_CODE
      *              INTO AN INTERNAL ARRAY FOR IMPROVING PERFORMANCE.
      *
      * NEEDED     : LIBLP/LP01LC.CPY   (NEEDS 78 LC-SIZE)
      *
      *=================================================================
      * REV:
      * MFB 2023-0926 ***NEW***
      * JKC 2023-1130 CHANGED TO MATCH UP TO THE OTHER ARRAY MEMBERS.
      *
      ******************************************************************
       77  ARRAYLC-ELE                 PIC 9(3)      VALUE ZEROES.
       77  ARRAYLC-MAX                 PIC 9(3)      VALUE ZEROES.

       01  ARRAYLC-LC1-KEY.
           03  ARRAYLC-LC-CODE         PIC 9(3).

       01  ARRAYLC-FILE.
           03  ARRAYLC-REC             PIC X(LC-SIZE)
                                           OCCURS 1 TO 500
                                           DEPENDING ON ARRAYLC-MAX.

       01  ARRAYLC-REC-READ            PIC X(LC-SIZE).

