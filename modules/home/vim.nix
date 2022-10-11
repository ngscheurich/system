# ----------------------------------------------------------
# The Vim text editor, plus configuration
# ----------------------------------------------------------

{ ... }:

{
  programs.vim = {
    enable = true;

    settings = {
      expandtab = true;
      hidden = true;
      ignorecase = true;
      modeline = true;
      number = true;
      relativenumber = true;
      shiftwidth = 2;
      smartcase = true;
      tabstop = 2;
      undofile = true;
    };

    extraConfig = ''
      let g:loaded_netrwPlugin      = 0
      let g:loaded_python_provider  = 0
      let g:loaded_python3_provider = 0
      let g:loaded_ruby_provider    = 0
      let g:loaded_node_provider    = 0
      let g:loaded_perl_provider    = 0

      syntax enable

      set cursorline
      set fillchars+=vert:│
      set foldlevel=99
      set foldmethod=indent
      set grepprg=rg\ --vimgrep
      set laststatus=2
      set noshowmode
      set noswapfile
      set scrolloff=10
      set shortmess+=c
      set signcolumn=yes
      set smartindent
      set softtabstop=2
      set splitbelow
      set splitright
      set termguicolors
      set updatetime=1000
    '';
  };
}
