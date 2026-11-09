# amanuensis

[![R-CMD-check](https://github.com/marcelopisuner/amanuensis-r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/marcelopisuner/amanuensis-r/actions/workflows/R-CMD-check.yaml) [![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

Beta: details may change before 1.0; please [report](https://github.com/marcelopisuner/amanuensis-r/issues) anything that looks wrong.

**Turn photographs and scans of historical documents into data, from R.**

An *amanuensis* is a scribe who copies documents for someone else. This
package gives you one: an AI model, Google's Gemini, reads each page and
writes down what you ask for, in a table you can open in Excel, Stata or R.

You describe what you want in two files:

- the **instructions**, a short text that says what the document is and how
  to read it;
- the **codebook**, a spreadsheet with one row for each piece of information
  you want (a surname, an age, an occupation...).

The package does the rest. It prepares the images, sends the pages to Gemini
one at a time, checks every answer against your codebook, tells you what it
costs, and saves the results. It can also tell you how accurate the results
are, by comparing them with your own transcription of a few pages.

This is not OCR. OCR gives you text, which you would still have to turn into a
table yourself; here the model reads the page and fills in the table at once.

Prefer Python? The same tool exists as a Python package:
[amanuensis-python](https://github.com/marcelopisuner/amanuensis-python).

## What you need

- **R**, version 4.2 or newer: <https://cran.r-project.org>
- **RStudio** (recommended, free): <https://posit.co/download/rstudio-desktop/>
- **A Google account**, to get a Gemini key (see below).
- **Your documents**: a PDF, or photographs and scans (JPEG, TIFF, PNG, HEIC
  from an iPhone, WEBP, JPEG 2000).

You do not need to know how to program. Every step below is a line to copy
into R: in RStudio, paste it into the Console and press Enter.

## Install

```r
install.packages("remotes")
remotes::install_github("marcelopisuner/amanuensis-r")
```

R may ask you questions while it installs:

- If it lists packages that have newer versions and asks which ones to
  update, type `1` (All) and press Enter. amanuensis needs recent versions of
  some of them.
- If it asks *"Do you want to install from sources...?"*, type `no` and press
  Enter.

When it has finished, restart R (in RStudio: **Session > Restart R**) and load
the package. Its name is just `amanuensis`:

```r
library(amanuensis)
```

Two optional packages add features: `haven` saves Stata files, and `shiny`
gives you an editor for the accuracy check. Install them now, or whenever you
need them:

```r
install.packages(c("haven", "shiny"))
```

On Linux, `magick` and `pdftools` need system libraries (ImageMagick and
Poppler); if they are missing, the installation says which ones to add.

Want to try it before you get a key? Go to the
[worked example](#a-worked-example-no-key-no-cost): it needs no key and costs
nothing.

## Your Gemini key

The key works like a password: it lets the package use Gemini through your
Google account.

1. Go to <https://aistudio.google.com/apikey>, sign in with your Google
   account and create a key. It is a long code that starts with `AIza`
   (newer keys start with `AQ.`). Copy it.

2. In R, run:

   ```r
   set_key()
   ```

   A window asks for the key: paste it (it stays hidden) and confirm. R
   then asks whether to save it on this computer: answer yes, and you will not
   need to do this again. The package also checks with Google that the key
   works.

3. Check that everything is ready:

   ```r
   check_setup()
   ```

   Lines marked `PROBLEM` must be fixed, and each one says how. Lines marked
   `NOTE` are worth reading, but they do not stop you.

Keep the key private: never put it in a script, an email or a shared
document. On a shared computer, `set_key(remove = TRUE)` deletes it when you
have finished.

**Free or paid?** Every page you transcribe is sent to Google. With a key from
a Google project without billing, Gemini is free, but Google may use what you
send to improve its products. For material you may not share (for example,
records with personal data), use a key from a project with billing switched
on. The costs that amanuensis shows are Google's paid prices.

## The workflow

Put your documents in one folder and tell R to work there. In RStudio:
**Session > Set Working Directory > Choose Directory...**. Or type the folder's
path, with `/` even on Windows:

```r
setwd("C:/research/padro1930")
```

The examples below use a PDF called `padro1930.pdf`: change the file names to
yours. Run the lines one at a time, because some of them ask you a question.

### Step 1. One image per page

```r
library(amanuensis)
pages <- images("padro1930.pdf")
```

R asks whether to save the page images in a new folder, `padro1930_pages`:
answer yes. A folder of photos works too: `images("photos")`. Your original
files are never changed. Type `pages` to see the list of pages.

### Step 2. The instructions

This writes a starter file in your folder:

```r
instructions_template("instructions.docx")
```

Open `instructions.docx` with Word, replace each text in `<angle brackets>`,
then save and close it. Keep it to about one page: what the document is, what
one page shows, and anything unusual about how it is written (abbreviations,
ditto marks, things to ignore). Then read it into R:

```r
prompt <- instructions("instructions.docx")
prompt
```

`prompt` shows exactly what the model will be told. The package adds the
standard transcription rules at the end for you: copy exactly what is written,
mark doubtful readings with `?`, leave illegible things empty, never invent.

### Step 3. The codebook

This writes a starter spreadsheet, an example for a list of people:

```r
codebook_template("codebook.xlsx")
```

Open `codebook.xlsx` with Excel, change it into your own variables (its
second sheet explains each column), then save and close it. It has one row
per variable, with these columns:

| Column | What to write |
|---|---|
| `variable` | A short name, such as `surname`. |
| `type` | `text`, `number`, `integer` (a whole number), `yes/no`, or `category` (one of a list of values). Keep dates and identifiers as `text`. |
| `description` | What the model should write, with an example as written in the document. Say what must never happen ("copy it, never compute it"). |
| `values` | For a `category` only: the allowed values, separated by `;`. |
| `level` | `row` for each person or entry (the usual case), or `page` for something written once on the page, such as a page number. |

Then read it into R:

```r
cb <- codebook("codebook.xlsx")
cb
```

`cb` shows the variables as the package understood them. If you want a plain
transcription of each page rather than a table, use
`cb <- codebook(free_text = TRUE)` instead.

### Step 4. Try a few pages

```r
res <- transcribe(pages, prompt, cb, limit = 3)
res
```

This reads 3 pages and shows the results. If the model gets something wrong,
make the instructions or the codebook clearer, read them into R again (steps 2
and 3), and run this line again.

### Step 5. Read every page

```r
res <- transcribe(pages, prompt, cb)
```

Pages already read in this R session, with the same instructions and
codebook, are not sent (or paid for) again. With 25 pages or more to read, R
reads the first two, shows what the rest would cost, and asks before going on.
To set a maximum in US dollars, add `budget = 5`. For a long run, add
`save_to = "results"`: each page is then saved in that folder as it arrives,
so you can close R and continue later by running the same line.

### Step 6. Save the results

```r
output(res, "padro1930.xlsx")
```

The Excel file holds your data, a sheet with every page's status and cost,
the codebook, and a record of how the run was made. Other formats work the
same way: `.csv`, `.dta` (Stata), `.docx` (Word), `.txt`, `.md`, `.json` and
`.rds` (R). For example:

```r
output(res, "padro1930.dta")
```

### Step 7. Check the accuracy

Before you trust the table, compare it with your own transcription of a few
pages. This draws 3 pages at random and gives you a file to fill in with
Excel (or, if shiny is installed, an editor in your browser):

```r
benchmark("gold.xlsx", pages, cb, n = 3, seed = 1930)
```

Type exactly what is written on those pages, following the same rules as the
model, without looking at what the model wrote for them. In Excel, write `yes`
in the column `done` of the sheet `pages` for each page you finish. Then:

```r
gold <- benchmark("gold.xlsx")
s <- score(gold, res)
s
View(s$differences)
```

`s` shows the share of rows, values and characters the model got right, and
`s$differences` lists every disagreement, cell by cell. The `seed` (here, the
year of the document) fixes which pages are drawn, so anyone can check the
draw. A few pages are enough to try the method; for a figure to publish, use
`n = 10` or more.

## A worked example (no key, no cost)

The package comes with a small made-up example: three pages of a fictional
1930 municipal register, with instructions, a codebook, a correct
transcription, and prepared answers (`model = "example"`) with a few mistakes
planted on purpose. You can paste the whole block into R at once:

```r
library(amanuensis)
ex <- example_project()           # copies the example into a temporary folder
ex$folder                         # where the files are

pages <- images(ex$scans, save_to = file.path(ex$folder, "pages"))
prompt <- instructions(ex$instructions)
cb <- codebook(ex$codebook)
cb                                # the variables, as the package understood them

res <- transcribe(pages, prompt, cb, model = "example")
res                               # one row per person in the register
output(res, table = "pages")      # every page: status, tokens, cost

output(res, file.path(ex$folder, "padro1930.xlsx"))

gold <- benchmark(ex$gold)
s <- score(gold, res)
s                                 # rows, values and characters right
View(s$differences)               # the planted mistakes, cell by cell
```

The temporary folder is deleted when you close R. To keep the files and open
them in Excel, give a folder of your own:
`ex <- example_project("C:/research/amanuensis-example")`.

With your key, leave out `model = "example"` and the same lines send the three
pages to Gemini for real. To practise on your own documents without a key,
use `model = "mock"`, which makes up answers at no cost.

## What each function does

| Function | What it does |
|---|---|
| `set_key()` | Stores your Gemini key, after asking you. |
| `check_setup()` | Checks that everything is ready. Changes nothing. |
| `images()` | Turns PDFs, photos and scans into one image per page. |
| `instructions()` | Reads your instructions (Word, text or Markdown) and adds the standard rules. |
| `codebook()` | Reads your codebook (Excel or CSV). |
| `transcribe()` | Sends every page to Gemini, checks every answer and keeps track of the cost. |
| `output()` | Saves the results as Excel, CSV, Stata, Word, text, JSON or R files. |
| `benchmark()` | Draws pages for you to transcribe by hand, and reads your transcription back. |
| `score()` | Compares the model's results with your transcription. |
| `gemini_models()` | The models the package knows, with their prices. |
| `instructions_template()`, `codebook_template()` | Starter files to fill in. |
| `example_project()` | Copies the example to practise with. |
| `cancel_batch()` | Cancels a batch job still running at Google. |

## Good to know

- **Faithful copies.** The model is told to copy exactly what is written
  (spelling, abbreviations, accents, and the document's own mistakes), to
  mark doubtful readings with `?`, to leave illegible things empty, and never
  to invent. The file and page each row comes from are recorded by the
  package, not by the model.
- **Your files.** Your original files are never changed. `transcribe()` saves
  nothing on your computer unless you give it a `save_to` folder: the results
  stay in R until you save them with `output()`. `images()` asks before it
  creates a folder you have not named.
- **Never paying twice.** If a run stops (you press Esc, the budget runs out,
  an error), the pages read so far are kept, and running the same line again
  sends only the missing ones. Without `save_to`, this lasts until you close
  R.
- **Cost.** The cost of every page is recorded: see it with
  `output(res, table = "pages")`. `budget = 5` stops a run before it spends
  more than $5. `batch = TRUE` costs half; it is slower, needs a key with
  billing, and needs a `save_to` folder.
- **Changing your mind.** Results read with other instructions, another
  codebook or another model are never mixed in one table.

## Help

Every function has a help page: type `?transcribe` (or another function's
name) in R. For anything else, or if something goes wrong,
[open an issue](https://github.com/marcelopisuner/amanuensis-r/issues).

## Licence

MIT. See [LICENSE.md](LICENSE.md).
