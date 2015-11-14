;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;; Parser for Yeastract service

(in-package #:bio-utils)

(require 'drakma)


(defparameter *yeastract-elements* (('promoter . "promoter_sequences.fasta.gz")
				    ('gene-seqs . "gene_sequences.fasta.gz")
				    ('aa-seqs . "aminoacid_sequences.fasta.gz")
				    ('reg-matrix . "RegulationMatrix_Documented_2013927.csv.gz")
				    ('reg-2colmns . "RegulationTwoColumnTable_Documented_2013927.tsv.gz")
				    ('transfac-list . "TFConsensusList_20130918.transfac.gz")))



