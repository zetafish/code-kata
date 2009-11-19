// http://www.spoj.pl/problems/SBANK/
// 
// Plain C implementation

#include <stdio.h>

#define MAX 100000
#define BLOCK 33

// Trie node
struct node {
  struct node* left;
  struct node* right;
  struct node* mid;
  int ch;
  int last;
};

struct node NODES[MAX * BLOCK];
struct node* root;
struct node* node_ptr;

struct node* create_node(int ch) {
  struct node* node = node_ptr++;

  node->ch = ch;
  node->left = node->right = node->mid = NULL;
  node->last = 0;
  return node;
};

struct node* create_trie(int* s) {
  struct node* node;
  struct node* p;

  p = node = create_node(*s);

  while (*++s >= 0) {
    p = p->mid = create_node(*s);
  }
  p->last = 1;
  return node;
}

struct node* insert(struct node* node, int* s) {
  int ch = *s;

  if (node == NULL) {
    node = create_trie(s);
  }
  else if (ch < node->ch) {
    node->left = insert(node->left, s);
  }
  else if (ch > node->ch) {
    node->right = insert(node->right, s);
  }
  else if (*(s+1) >= 0 ) {
    node->mid = insert(node->mid, s+1);
  }
  else {
    // last character.
    ++node->last;
  }
  return node;
}


int read_int()
{
  char s[10];
  return atoi(gets(s));
}

void show_trie(struct node* node, int pos) {
  static int g[6];

  if (!node) {
  }
  else if (node->last) {
    printf("%02d %08d %04d %04d %04d %0d4 %d\n", 
	   g[0], g[1], g[2], g[3], g[4], g[5], node->last);
  }
  else {
    int old = g[pos];

    show_trie(node->left, pos);
    
    g[pos] = node->ch;
    show_trie(node->mid, pos+1);
    g[pos] = old;

    show_trie(node->right, pos);
  }
}

void read_account(int* g)
{
  static char s[100];
  gets(s);
  sscanf(s, "%d %d %d %d %d %d", g, g+1, g+2, g+3, g+4, g+5);
  g[6] = -1;
}

void solve() {
  int n = read_int();
  int i = 0;
  char s[100];
  int g[7];

  node_ptr = NODES;
  root = NULL;
  while (n-- > 0) {
    read_account(g);
    root = insert(root, g);
  }

  show_trie(root, 0);
}

int main(int argc, char** argv) {
  int k = read_int();
  int i;
  for (i=0 ; i<k ; ++i) {
    if (i>0) {
      read_int(); // just skip a line
      printf("\n"); // add a newline
    }
    solve();
  }
  return 0;
}
