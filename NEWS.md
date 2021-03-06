# mikropml 1.0.0

- mikropml now has a logo created by @NLesniak!
- Made documentation improvements (#238, #231 @kelly-sovacool; #256 @BTopcuoglu).
- New option in `preprocess_data()`: `prefilter_threshold` (#240, @kelly-sovacool, @courtneyarmour).
    - Remove any features that appear in N=`prefilter_threshold` or fewer rows in the data.
    - Created function `remove_singleton_columns()` called by `preprocess_data()` to carry this out.
- New option in `get_feature_importance()`: `groups` (#246, @kelly-sovacool).
    - Provide custom groups of features to permute together during permutation importance.
    - `groups` is `NULL` by default; in this case, correlated features above `corr_thresh` are grouped together.
- `preprocess_data()` now replaces spaces in the outcome column with underscores (#247, @kelly-sovacool, @JonnyTran).
- Clarify in the intro vignette that we do not support multi-label outcomes. (#254, @zenalapp)
- Optional progress bar for `preprocess_data()` and `get_feature_importance()` using [the progressr package](https://github.com/HenrikBengtsson/progressr) (#257, @kelly-sovacool, @JonnyTran, @FedericoComoglio).
- The mikropml paper is soon to be published in [JOSS](https://joss.theoj.org/papers/72bf31a3f51f8fc273ef6b99bd04ede1)!

# mikropml 0.0.2

- Fixed a test failure on Solaris.
- Fixed multiple test failures with R 3.6.2 due to `stringsAsFactors` behavior.
- Made minor documentation improvements.
- Moved `rpart` from Suggests to Imports for consistency with other packages used during model training.

# mikropml 0.0.1

This is the first release version of mikropml! 🎉

- Added a `NEWS.md` file to track changes to the package.
- Major new functions:
    - `run_ml()`
    - `preprocess_data()`
    - `plot_model_performance()`
    - `plot_hp_performance()`
- Support for ML methods in `run_ml()`:
    - `glmnet`: logistic and linear regression
    - `rf`: random forest
    - `rpart2`: decision trees
    - `svmRadial`: support vector machines
    - `xgbTree`: gradient-boosted trees
- New vignettes:
    - [Introduction](http://www.schlosslab.org/mikropml/articles/introduction.html)
    - [Preprocess data](http://www.schlosslab.org/mikropml/articles/preprocess.html)
    - [Hyperparameter tuning](http://www.schlosslab.org/mikropml/articles/tuning.html)
    - [Parallel processing](http://www.schlosslab.org/mikropml/articles/parallel.html)
    - [The mikropml paper](http://www.schlosslab.org/mikropml/articles/paper.html)
