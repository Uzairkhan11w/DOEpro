###############################################################################
##  GUIDES
###############################################################################
## The guidance that used to sit on a Help tab of its own, each part now in
## the tab where it is needed, folded away until it is opened. How to report
## the results is in the interpretation itself (its last section).

## a folded guide: a one-line title that opens to the text
tab_guide <- function(title, html)
  tags$details(class = "slx-guide", tags$summary(title), HTML(html))

GUIDE_START <- "
<ol>
<li>Put your data in <b>long format</b> - one row per plot, one column per variable - and paste it in on the Data tab.</li>
<li>Choose the design and map your columns to their roles. You may select <b>several response variables at once</b>; each is analysed separately with the same design and appears side by side in the tables of means.</li>
<li>The app screens every numeric column and says which transformation, if any, it needs. Press <i>Apply all suggested</i> to accept the advice.</li>
<li>Press <b>Run analysis</b>.</li>
</ol>
"

GUIDE_DESIGNS <- "
<table class='doe'>
<tr><th>Design</th><th>Columns required</th><th>Error term(s)</th></tr>
<tr><td>CRD</td><td>treatment, response</td><td>single pooled error</td></tr>
<tr><td>RCBD</td><td>block, treatment, response</td><td>single pooled error</td></tr>
<tr><td>Latin square</td><td>row, column, treatment, response</td><td>single pooled error</td></tr>
<tr><td>Factorial CRD / RCBD</td><td>(block,) factors A-D, response</td><td>single pooled error</td></tr>
<tr><td>Split plot</td><td>replication, main plot, sub plot, response</td><td>Error(a), Error(b)</td></tr>
<tr><td>Strip plot</td><td>replication, factor A, factor B, response</td><td>Error(a), Error(b), Error(c)</td></tr>
<tr><td>Pooled over environments (RCBD)</td><td>environment, replication, treatment, response</td><td>R(env), pooled error</td></tr>
<tr><td>Pooled over environments (CRD)</td><td>environment, treatment, response</td><td>pooled error</td></tr>
<tr><td>Pooled factorial over environments (2-4 factors)</td><td>environment, (replication,) factors A-D, response</td><td>each effect vs its environment interaction; pooled error</td></tr>
</table>
"

GUIDE_POOLED <- "
<p>When the <b>same experiment is repeated across several environments</b> - locations,
years, seasons or other groups - a combined analysis tests the treatments, the
environments, and the treatment &times; environment interaction together. Arrange the data
with one column identifying the environment, alongside the usual replication and treatment
columns, and stack all environments in one long table.</p>
<p>The analysis proceeds in three steps:</p>
<ol>
<li><b>Homogeneity of error variances.</b> Bartlett's test compares the error variances of
the separate environments. If the test finds no significant difference at your chosen level,
the environments may be pooled; if it does, the app warns you and a variance-stabilising
transformation (or separate analyses) should be considered. The verdict is shown above the
ANOVA table.</li>
<li><b>Combined ANOVA</b> with the correct error terms:
  <ul>
  <li><b>RCBD base:</b> Environment is tested against replications-within-environment; the
  treatment is tested against the treatment &times; environment interaction; the interaction
  is tested against the pooled error.</li>
  <li><b>CRD base:</b> Environment and the interaction are tested against the pooled error;
  the treatment is tested against the interaction.</li>
  </ul>
  Testing the treatment against the interaction (rather than the pooled error) is the
  essential feature of a combined analysis - it asks whether a treatment's advantage is
  consistent enough across environments to be declared real.</li>
<li><b>Means and critical differences.</b> The treatment table gives means <b>averaged over
all environments</b>, with the critical difference built from the interaction mean square.
The environment &times; treatment table compares treatments <b>within a single environment</b>
using the pooled error, and also reports the standard error for comparing a treatment's
overall mean across environments.</li>
</ol>
<table class='doe'>
<tr><th>Comparison</th><th>Standard error of a difference</th></tr>
<tr><td>two treatment means, over all environments</td><td>&radic;(2&middot;M<sub>TxE</sub> / re)</td></tr>
<tr><td>two environment means</td><td>&radic;(2&middot;M<sub>error</sub> / rt) &nbsp;(CRD) &nbsp;or&nbsp; &radic;(2&middot;M<sub>R(E)</sub> / rt) &nbsp;(RCBD)</td></tr>
<tr><td>two treatments in the same environment</td><td>&radic;(2&middot;M<sub>error</sub> / r)</td></tr>
</table>
<p><b>Factorial treatments over environments.</b> When the treatments themselves form a
2-, 3- or 4-factor factorial, choose one of the <i>Pooled factorial over environments</i>
designs and set the number of treatment factors. The combined analysis then partitions the
treatment variation into every main effect and interaction, and applies the same rule
throughout: <b>each treatment effect (a main effect or an interaction among the treatment
factors) is tested against its own interaction with the environment</b>, and every
environment &times; treatment interaction is tested against the pooled error. Each effect's
table of means, pooled over environments, uses the critical difference built from that
effect's environment interaction. This tells you which main effects and interactions are
stable enough across environments to be declared real.</p>
"

GUIDE_ERRORS <- "
<p>With <i>n</i> observations behind each mean, SE(m) = &radic;(MSE/n),
SE(d) = &radic;(2&middot;MSE/n) and C.D. = t<sub>&alpha;/2, df</sub> &times; SE(d), where
&alpha; is the significance level you chose.</p>
<p>When replication is unequal, mean <i>i</i> has SE(m) = &radic;(MSE/n<sub>i</sub>) and
two means have SE(d) = &radic;(MSE(1/n<sub>i</sub> + 1/n<sub>j</sub>)), so the C.D. depends
on which two means are compared and the tables show its range. In a blocked design with a
missing plot, or a factorial with unequal cells, the means are adjusted (least-squares)
means and their standard errors come from the fitted model. Split plots, strip plots and
pooled analyses must be complete: with a plot missing the app says what is missing rather
than give an approximate answer.</p>
<p>A split plot needs <b>four</b> different SE(d):</p>
<ul>
<li>two main-plot means: &radic;(2&middot;Ea / rb)</li>
<li>two sub-plot means: &radic;(2&middot;Eb / ra)</li>
<li>two sub-plot means within the same main plot: &radic;(2&middot;Eb / r)</li>
<li>two main-plot means at the same sub-plot level: &radic;(2[(b-1)Eb + Ea] / rb), with a weighted <i>t</i></li>
</ul>
<p>A strip plot needs three error terms and the analogous mixed comparisons. The app
prints every one of them under the relevant table of means, so you never have to work
out which C.D. belongs to which comparison.</p>
"

GUIDE_TRANSFORM <- "
<ul>
<li><b>Counts</b> (insects, spores, grains): &radic;y, or &radic;(y+0.5) when zeros occur. Signature: integer data whose variance rises in proportion to the mean, i.e. Taylor slope b &asymp; 1.</li>
<li><b>Percentages and proportions</b>: the angular, or arcsine square-root, transformation. A 0-100 range on its own is <i>not</i> evidence - most yields live there too - so the app also looks at the column name, and you can declare the type yourself.</li>
<li><b>Variance proportional to the square of the mean</b> (b &asymp; 2): log y, or log(y+1) when zeros occur.</li>
<li><b>Variance rising faster still</b> (b &gt; 2.5): 1/y.</li>
<li>Otherwise the Box-Cox profile likelihood chooses &lambda;.</li>
</ul>
<p>Means are always <b>back-transformed</b> for presentation. SE, C.D. and C.V. stay on the
transformed scale, because that is where the tests were performed; tables therefore show
the back-transformed mean with the transformed value in parentheses.</p>
"

GUIDE_POSTHOC <- "
<ul>
<li><b>Least significant difference (LSD), Fisher's protected</b>: only after a significant F-test, and best with few treatments.</li>
<li><b>Tukey's honestly significant difference (HSD)</b>: controls the error rate over all pairwise comparisons; the safe default.</li>
<li><b>Duncan's multiple range test (DMRT)</b>: less conservative, still standard in agronomy.</li>
<li><b>Student-Newman-Keuls (SNK) test</b>: sits between Duncan and Tukey.</li>
<li><b>Scheffe's test</b>: the most conservative; built for arbitrary contrasts.</li>
<li><b>Bonferroni-adjusted LSD</b>: simple and strict.</li>
<li><b>Dunnett's test</b>: compares each treatment with a control you choose, two-sided or one-sided (higher, or lower, than the control); the right test when the control is the only comparison of interest.</li>
</ul>
<p>All of them are computed from the error mean square of whichever effect you select, so in a
split or strip plot they automatically use the right error stratum. An interaction in a split
plot, strip plot or pooled analysis is compared within one level of the main-plot (or strip)
factor, or one environment, at a time. With unequal replication each pair of means uses its
own SE(d): Tukey's HSD becomes the Tukey-Kramer procedure, and Duncan's and the
Student-Newman-Keuls tests use Kramer's adjustment. As everywhere in the app, no letters are
shown when the effect's F-test is not significant.</p>
"
