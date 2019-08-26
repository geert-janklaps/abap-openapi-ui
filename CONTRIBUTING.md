# General Guidelines

*  Before starting any significant development, open an issue and propose your solution first. A discussion can save a lot of unnecessary work. It also helps others know that this is being worked on.
*  It is in your best interest to keep the commits/PRs as small as possible and solve one thing at a time. The smaller your change is, the easier it is to review and it will be more likely to get accepted.
*  Commit often, whenever something is working, and is a step in the right direction do a commit or PR. This way other contributors can see the changes, and it will minimize the risk of merge conflicts.
*  If you don't have the time or knowledge to fix the problem yourself, you can still make it move along faster by providing an accurate description.

# Bug Reports
Guidelines for bug reports:

1.  **Use the Gitlab issue search** — check if the issue has already been reported.
2.  **Check if the issue has been fixed** — try to reproduce it using the latest master or development branch in the repository.
3.  **Demonstrate the problem** — provide clear steps that can be reproduced.

A good bug report should not leave others needing to chase you up for more information. Please try to be as detailed as possible in your report. What is your environment? What steps will reproduce the issue? What would you expect to be the outcome? All these details will help to fix any potential bugs.

# Development Guidelines
**Prefixing**

Variables are prefixed using the standard setting in [abapOpenChecks Naming Conventions](http://docs.abapopenchecks.org/checks/69/).

**Downport**

ABAP OpenAPI UI is targeted for version 740 (sp05), so the code should only contain expressions/statements that works on 740 (sp05). 
[abaplint](https://github.com/abaplint/abaplint) will automatically check every PR for language syntax that is not available on 740 (sp05).

**Pretty Printer**

Use pretty printer, keywords upper case + indentation, [abapOpenChecks](http://docs.abapopenchecks.org/checks/06/) can be used for checking this.