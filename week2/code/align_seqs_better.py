#!/usr/bin/env python3

"""Use functions to find the longest matching sequence in two files of DNA sequences"""

__author__ = 'Pu Zhao (pu.zhao23@imperial.ac.uk)'
__version__ = '0.0.1'

import sys
import pickle


def read_file(file1_name, file2_name):
    with open(file1_name) as f:
        next(f)
        s1 = f.read().strip().replace("\n", "")
    with open(file2_name) as f:
        next(f)
        s2 = f.read().strip().replace("\n", "")
    l1 = len(s1)
    l2 = len(s2)
    if l2 > l1:
        temp = s1
        s1 = s2
        s2 = temp
    return s1, s2, l1, l2


def calculate_score(s1, s2, l1, l2, startpoint):
    matched = ""  # to hold string displaying alignments
    score = 0
    for i in range(l2):
        if (i + startpoint) < l1:
            if s1[i + startpoint] == s2[i]:  # if the bases match
                matched = matched + "*"
                score = score + 1
            else:
                matched = matched + "-"

    # some formatted output
    # print("." * startpoint + matched)
    # print("." * startpoint + s2)
    # print(s1)
    # print(score)
    # print(" ")
    return score


def compare_seq_score(s1, s2, l1, l2):
    best_align = {}
    my_best_align = None
    my_best_score = -1
    for i in range(l1):  # Note that you just take the last alignment with the highest score
        z = calculate_score(s1, s2, l1, l2, i)
        if z >= my_best_score:
            my_best_align = "." * i + s2  # think about what this is doing!
            my_best_score = z
            best_align[my_best_align] = my_best_score

    # print(my_best_align)
    # print(s1)
    # print("Best score:", my_best_score)
    # s_total, i = [calculate_score(s1, s2, l1, l2, i) for i in range(bg_st_p)]
    return best_align


def out_put(dir_best_align):
    out_put_file = "../results/align_seqs_fasta2.p"
    with open(out_put_file, "wb") as f:
        pickle.dump(dir_best_align, f)
        f.close()
    # with open(out_put_file, "rb") as f:
    #     read_b_file = pickle.load(f)
    #     f.close()


def main(file1, file2):
    seq1, seq2, l1, l2 = read_file(file1, file2)
    dir_best_align = compare_seq_score(seq1, seq2, l1, l2)
    out_put(dir_best_align)

    return 0


if __name__ == "__main__":
    if len(sys.argv) == 3:
        status = main(sys.argv[1], sys.argv[2])
        sys.exit(status)
    elif len(sys.argv) == 2:
        status = main(sys.argv[1], "../data/fasta/407228326.fasta")
        sys.exit(status)
    elif len(sys.argv) == 1:
        status = main("../data/fasta/407228412.fasta", "../data/fasta/407228326.fasta")
        sys.exit(status)
    else:
        print("Wrong input, need 1 py file and 2 fasta files")
        sys.exit(1)
