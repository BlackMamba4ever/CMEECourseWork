#!/usr/bin/env python3

"""Find the best alignment and its corresponding score in an input file"""

__author__ = 'Pu Zhao (pu.zhao@imperial.ac.uk)'
__version__ = '0.0.1'

import sys
import csv


def read_file(filename):
    """
    Read a file and find the long sequence and the short sequence.
    """
    with open(filename, 'r') as f:
        # s1 = f.readline().strip()
        # s2 = f.readline().strip()
        csv_file = csv.reader(f)
        temp = []
        for row in csv_file:
            temp.append(row[1])
        s1 = temp[0]
        s2 = temp[1]
        print(type(s1))
        print(s2)
    l1 = len(s1)
    l2 = len(s2)
    if l2 > l1:
        temp = s1
        s1 = s2
        s2 = temp

    return s1, s2, l1, l2


def calculate_score(s1, s2, l1, l2, startpoint):
    """
    Use short sequence align the long one and return the alignment score.
    """

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
    """
    Compare the score of the matches by using calculate_score function.
    """

    my_best_align = None
    my_best_score = -1
    for i in range(l1):  # Note that you just take the last alignment with the highest score
        z = calculate_score(s1, s2, l1, l2, i)
        if z > my_best_score:
            my_best_align = "." * i + s2  # think about what this is doing!
            my_best_score = z
    # print(s1)
    # print("Best score:", my_best_score)
    # s_total, i = [calculate_score(s1, s2, l1, l2, i) for i in range(bg_st_p)]
    return my_best_score, my_best_align


def out_put(my_best_score, my_best_align):
    """
    Output the best alignment and its corresponding score.
    """

    out_put_file = "../results/align_seqs.txt"
    with open(out_put_file, "w") as f:
        f.write(my_best_align)
        f.write(":")
        f.write(str(my_best_score))


def main(file_name):
    seq1, seq2, l1, l2 = read_file(file_name)
    best_score, best_align = compare_seq_score(seq1, seq2, l1, l2)
    out_put(best_score, best_align)
    return 0


if __name__ == "__main__":
    if len(sys.argv) == 1:
        status = main("../data/seqfile.csv")
        sys.exit(status)
    else:
        print("Wrong input! Try again, need to provide a file")
        sys.exit(1)
