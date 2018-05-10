# library

SET_LOAD_LIB <- c("knitr", "readr", "dplyr", "stringr", "stringi", "lazyeval", "Rcpp", "BH", "R6", "DiagrammeR", "gtools")
sapply(X = SET_LOAD_LIB, FUN = library, character.only = TRUE, logical.return = TRUE)
knitr::opts_chunk$set(comment = NA)

# CaboChaのC++ APIのRcppコード
# 出力フォーマットはlattice(-f1)
# IOB2形式で固有表現タグを出力(-n1)
# http://taku910.github.io/cabocha/
rcpp_src <- '
DataFrame executeCabocha(SEXP str) {

using namespace Rcpp;
using namespace CaboCha;

std::string input = Rcpp::as<std::string>(str);

std::vector<int> link;
std::vector<unsigned short int> mnum, chunk_id;
std::vector<std::string> surface, feature, ne;

// CaboChaパーサーを生成して、入力文の係り受け解析結果を受け取る
Rcpp::XPtr<const CaboCha::Tree> tree(CaboCha::createParser("-f1 -n1")->parse(input.c_str()));
for (auto i : boost::irange(0, int(tree.get()->size()))) {
//    Rcpp::XPtr<const CaboCha::Token> token(tree.get()->token(i));
mnum.push_back(i);
if (tree.get()->token(i)->chunk != NULL) {
chunk_id.push_back(chunk_id.size() != 0 ? chunk_id.back() + 1: 0);
link.push_back(tree.get()->token(i)->chunk->link);
} else {
chunk_id.push_back(chunk_id.size() != 0 ? chunk_id.back() : 0);
link.push_back(link.back());
}

surface.push_back(std::string(tree.get()->token(i)->surface));
feature.push_back(std::string(tree.get()->token(i)->feature));
ne.push_back(std::string(tree.get()->token(i)->ne ? tree.get()->token(i)->ne : "O"));
}

return Rcpp::DataFrame::create(
Rcpp::Named("mnum") = mnum,
Rcpp::Named("chunk_id") = chunk_id,
Rcpp::Named("link") = link,
Rcpp::Named("surface") = surface,
Rcpp::Named("feature") = feature,
Rcpp::Named("ne") = ne
);
}
'

Sys.setenv("PKG_LIBS" = "-lmecab -lcabocha")
Sys.setenv("PKG_CXXFLAGS" = "-O2")
# Sys.getenv("PKG_LIBS")
executeCabocha <- Rcpp::cppFunction(
  code = rcpp_src,
  includes = c("#include <cabocha.h>", "#include <boost/range/irange.hpp>"),
  plugins = "cpp11"
)


# CaboChaの出力を整形
runCaboCha <- function (str){
  cabocha_res <- executeCabocha(str = as.character(str))
  return(
    dplyr::data_frame(
      mnum = cabocha_res$mnum,
      chunk_id = cabocha_res$chunk_id,
      link = cabocha_res$link,
      surface = as.character(cabocha_res$surface),
      feature = as.character(cabocha_res$feature),
      ne = as.character(cabocha_res$ne)
    )
  )
}

# パイプ処理の結果を文字列に変換する関数
# 同じことを何度かしたので共通化(前回定義した関数を流用)
convertToChar <- function (tgt, ...) {
  if (any(is.element(class(tgt), c("list", "data.frame", "tbl_df")))) {
    return(
      tgt %>%
        unlist %>%
        as.character
    )
  } else {
    return(as.character(tgt)) 
  }
}



# 第5章の入力データURL（固定）
TASK_INPUT_URL <- "http://www.cl.ecei.tohoku.ac.jp/nlp100/data/neko.txt"

# 複数の課題で必要とされるファイル名
TASK_FILE_NAME <- "neko.txt.cabocha"


# ファイル取得して各行毎にCaboChaで係り受け解析
download.file(
  url = TASK_INPUT_URL, destfile = basename(TASK_INPUT_URL), 
  method = "wget", quiet = FALSE
)
if (file.exists(file =  basename(TASK_INPUT_URL))) {
  readr::read_lines(
    file = basename(TASK_INPUT_URL), n_max = -1
  ) %>%
    na.omit() %>%
    dplyr::data_frame(str = .) %>%
    dplyr::rowwise(.) %>% 
    dplyr::do(., runCaboCha(str = .$str)) %>%
    readr::write_tsv(
      x = ., path = TASK_FILE_NAME, 
      col_names = TRUE, append = FALSE
    )
} else{
  stop("File not found.") 
}

