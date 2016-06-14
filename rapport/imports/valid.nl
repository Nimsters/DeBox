\begin{tabbing}
example:\\
From the premise (p\(\ra\)q)\(\land\)(p\(\ra\)r), we wish to prove p\(\ra\)q\(\land\)r.\\
We have the premise (p\(\ra\)q)\(\land\)(p\(\ra\)r) [prm].\\
By applying the first and-elimination rule to [prm], we get p\(\ra\)q [piq].\\
By applying the second and-elimination rule to [prm], we get p\(\ra\)r [pir].\\
Ass\=ume \+ p [p].\\
    By applying the implication-elimination rule to [p] and [piq], we get q [q].\\
    By applying the implication-elimination rule to [p] and [pir], we get r [r].\\
    By applying the and-introduction rule to [q] and [r], we get q\(\land\)r [qar].\\
\< \- Discharge assumption [p].\\
By applying the implication-introduction rule to [p]-[qar], we conclude p\(\ra\)q\(\land\)r.
\end{tabbing}
