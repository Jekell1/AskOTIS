      ******************************************************************
      *
      * IDENTIFICATION: LIBLP/LPWKINDEX.CPY
      *
      * DESCRIPTION   : SUBSCRIPTS FOR PREVIOUS INDEXES
      *                 REMOVED THE USE OF 'INDEXED BY' ON OCCURS
      *                 FOR USE WITH MICROFOCUS.
      *
      * NEEDED FOR    : LIBLP/LPWKCEP.CPY
      *                 LIBLP/LPWKCEPL.CPY
      *                 LIBLP/LPWKCIP.CPY
      *                 LIBLP/LPWKCIPL.CPY
      *                 LIBLP/LPWKCMP.CPY
      *                 LIBLP/LPWKCMPL.CPY
      *                 LIBLP/LPWKCOP.CPY
      *                 LIBLP/LPWKCOPL.CPY
      *
      *=================================================================
      * REV:
      * KEC 2015.0612 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         ***NEW*** REMOVED THE USE OF 'INDEXED BY' ON OCCURS
      *         FOR USE WITH MICROFOCUS.  THIS WAS CREATED TO ADDED
      *         INDEXES AS SUBSCRIPTS.
      *
      ******************************************************************
       01  LPWKINDEX-WORKERS           VALUE ZEROES.
           03  CEPIX                   PIC 9(01).
           03  CIPIX                   PIC 9(01).
           03  CMPIX                   PIC 9(01).
           03  COPIX                   PIC 9(01).

