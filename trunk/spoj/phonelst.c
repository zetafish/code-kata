// http://www.spoj.pl/problems/PHONELST/
// 
// Plain C implementation

#include <stdio.h>

struct node {
  struct node* left;
  struct node* right;
  struct node* mid;
  char ch;
  char last;
};

struct node NODES[10000 * 10];
struct node* root;
struct node* node_ptr;
int consistent;

struct node* create_node(char ch) {
  struct node* node = node_ptr++;
  node->ch = ch;
  node->left = node->right = node->mid = NULL;
  node->last = 0;
  return node;
};

struct node* create_trie(char* s) {
  struct node* node = create_node(*s);
  struct node* p = node;
  while (*++s) {
    p = p->mid = create_node(*s);
  }
  p->last = 1;
  return node;
}

struct node* insert(struct node* node, char* s) {
  char ch = *s;

  if (node == NULL) {
    node = create_trie(s);
  }
  else if (ch < node->ch) {
    node->left = insert(node->left, s);
  }
  else if (ch > node->ch) {
    node->right = insert(node->right, s);
  }
  else if (*(s+1)) {
    if (node->last) {
      consistent = 0;
    }
    else {
      node->mid = insert(node->mid, s+1);
    }
  }
  else {
    consistent = 0;
  }
  return node;
}


int read_number()
{
  char s[10];
  return atoi(gets(s));
}


void solve() {
  int n = read_number();
  char s[20];
  
  root = NULL;
  node_ptr = NODES;
  consistent = 1;
  while (n-- > 0) {
    gets(s);
    if (consistent) {
      root = insert(root, s); 
    }
  }
  puts(consistent ? "YES" : "NO");
}

int main(int argc, char** argv) {
  int n = read_number();
  while (n-- > 0) {
    solve();
  }
  return 0;
}

