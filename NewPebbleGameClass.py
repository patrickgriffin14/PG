#######################################################
# Title: 'pebblegame2 class' used in the main algorithm 
# Author: Patrick Griffin 11355826 					    
# Date: 09/11/2014 									    
#######################################################

import operator
import string
from sage.graphs.digraph import DiGraph
from sage.plot.colors import rainbow
class PebbleGraph(DiGraph):
	""" The Pebble graph class inherits methods from
	sage's DiGraph class in order tp construct
	the pebble games. This initialisation initialises
	all the properties needed to run all four
	pebble game variants defined as methods in this
	class. The first 14 methods in this class are
	only ment to be used inside this class by the
	pebble game methods which are the last four
	methods in this class. The class has parameters 
	'(k,l)'' being the sparsity that you would like to
	check for, 'G' which is the undirected input graph,
	and an optional parameter 'weight', which is used
	for when the weighted pebble game is the algorithm
	of choice."""									
	
	def __init__(self,k,l,G,weight=[]):
		self.rain = rainbow(200)
		self.weight= sorted(weight, key=operator.itemgetter(1))
		self.weight.reverse()
		self.alpha=string.lowercase
		G.graphplot(save_pos=True)  
		self.graphlst=[]			 							
		self.l=l 							
		self.k=k 					
		self.orig=G.copy()  		 	
		self.G=G.copy()              	
		G=G.to_directed() 				
		self.graph=G 					
		self.colour_edge={} 			
		self.colour_vertex={} 		                 			
		self.compindex=[] 			
		self.components=[]										  
		self.graph.delete_edges(self.graph.edge_iterator())    
		self.nodes=len(self.graph)							      
		self.pebs =[] 										 						
		self.red = 0

		for i in range(len(self.graph)):
			self.compindex.append([])
			self.colour_vertex[i]=[]
			for j in range(len(self.graph)):
				self.compindex[i].append(0)
			for col in range(k):
				self.colour_vertex[i].append(self.alpha[col])
		
		for i in range(len(self.graph)):
		 	self.pebs.append(k)
		 	
		
	def add_edge1(self,i,j):
		""" This method adds an edge between two vertices i and j.
		And also removes a pebble from i.""" 									
		if(self.pebs[i]+self.pebs[j] >= self.l+1): 	  	   
			self.graph.add_edge(i,j) 					 
			self.pebs[i]=self.pebs[i]-1 				
		else:
			return False 	

	
	def swap_edges(self,i,j):
		""" This method reverses a list of edges in D, whilst also
		accounting for the movement of pebbles. """ 																	
		lst=[] 																					 	 
		for x in range(len(self.graph.shortest_path(i,j))-1):  									   
			lst.append([self.graph.shortest_path(i,j)[x],self.graph.shortest_path(i,j)[x+1]])	
	
		self.graph.reverse_edges(lst) 														        
		self.pebs[j]=self.pebs[j]-1 															 										 
		self.pebs[i]=self.pebs[i]+1 		
		if(len(self.components)!=0):  															
			self.colour_comp(self.components)
		self.graphlst.append(self.graph.copy())
		self.colour_edge.clear()
		return True
	
	
	def search(self,vertex1,vertex2,p):
		""" The search and gather methods are used simultaneously
		to gather pebbles at the vertices in order to add an edge
		between. """
		p.remove(p[0]) 													   
		if(len(p)==0): 													
			return False 												 
		elif(len(p)!=0 and self.pebs[p[0]]==0  or p[0] == vertex2): 	
			self.search(vertex1,vertex2,p)
		else: 								 							 					 
			if self.swap_edges(vertex1,p[0]): 						
				return True 					
			
	
	def gather(self,node1,node2):
		if(self.pebs[node1]<=self.pebs[node2]):  						
			dfs = list(self.graph.depth_first_search(node1))          
			if(self.search(node1,node2,dfs)): 						    
				return True								   
			else:
				return False
		else:
			dfs1 = list(self.graph.depth_first_search(node2))
			if(self.search(node2,node1,dfs1)):
				return True
			else:
				return False

	
		"""The next five methods are used only in the 
	 	PGcomp method for doing the components
	 	pebble game."""

	
	def reach(self,u,v):
		vert1=list(self.graph.depth_first_search(u))
		vert2=list(self.graph.depth_first_search(v))
		return list(set(vert1)|set(vert2))

	
	def colour_comp(self,comp,i=0,j=0,k=0):
		""" This method is responsible for colouring the components
		at each step of the game. """
		if(i==len(comp)): 		 
			return True			
 		elif(j==0 and k==0):    
			self.colour_edge[self.rain[i*30]]=[]
			return self.colour_comp(comp,i,j,k+1)
		elif(j==len(comp[i])):
			return self.colour_comp(comp,i+1,0,0)
		elif(k==len(comp[i])):
			return self.colour_comp(comp,i,j+1,0)
		else:
			if self.graph.has_edge(comp[i][j],comp[i][k]):
				self.colour_edge[self.rain[i*30]].append((comp[i][j], comp[i][k]))
				return self.colour_comp(comp,i,j,k+1)
			else:
				return self.colour_comp(comp,i,j,k+1)	
		
	
	def compdet(self,u,v):
		""" This method is used to detect if there are
		any components being induced in the graph D as the
		components pebble game is being played. """
		c=self.reach(u,v)
		p=list(c)
		p.remove(u) 		
		p.remove(v)
		if(self.pebs[u]+self.pebs[v]>self.l):
			return 0 		
		if(len(p)!=0): 		
			 for x in p: 	
				if(self.pebs[x]!=0):
					return 0
			
		D=self.graph.copy()
		D.reverse_edges(self.graph.edges())
		vert = list(D.vertices())
		for j in c:
			vert.remove(j)
		dfs=set()		
		for i in vert:
			if(self.pebs[i]!=0):
				y=set(D.depth_first_search(i))
				dfs=dfs|y
		compo=set(self.graph.vertices())-dfs
		self.comp_maintenance(compo)
		return compo	

	
	def make_comp(self,u,v):
		""" This method updates the components reference matrix
		so the main algorithm can quickly check if two vertices,
		which an edge wants to be added between, lies in the same
		comoponent."""
		t=self.compdet(u,v)
		if(t==0):
			self.compindex[u][v]=0
			self.compindex[v][u]=0
		elif(len(t)==2):
			self.compindex[u][v]=1
			self.compindex[v][u]=1
		else:
			for i in t:
				for j in t:
					if(i!=j):
						self.compindex[i][j]=1	
			
			
	def comp_maintenance(self,comp):
		""" This method updates the list of components
		of the graph D whilst the components pebble game
		is being played."""
		if(len(self.components)==0):
			self.components.append(list(comp))
			self.colour_comp(self.components)
		else:
			lst=[]
			for i in self.components:
				if(set(i)|comp==comp):
					lst.append(set(i))
			for i in lst:
				self.components.remove(list(i))
			self.components.append(list(comp))	
			self.colour_comp(self.components)	

	
	
	def monochrom1(self,c,path,edge_colors,i=0,j=1,k=0):
		""" This method intakes a list of paths and determines
		whether any of them are monochromatic and returns the
		right one if it exists."""
		if(k==len(path)):
			return False
		elif(j==len(path[k])):
			return path[k]
		elif(edge_colors.edge_label(path[k][i],path[k][j]) in c):
			return self.monochrom1(c,path,edge_colors,i+1,j+1,k)
		else:
			return self.monochrom1(c,path,edge_colors,0,1,k+1)

	
	def edge_swap(self,path):
		""" This method is the coloured pebble games version
		of edge swapping. It has to take into account the edges 
		colour change when reversing them. The edge colours are
		distinguished by there edge label."""
		for i in range(len(path)-1):
			self.graph.reverse_edge(path[i+1],path[i])
			self.colour_vertex[path[i+1]]+=[self.graph.edge_label(path[i],path[i+1])]
			if(self.graph.edge_label(path[i],path[i+1]) in self.colour_vertex[path[i]]):
				self.colour_vertex[path[i]].remove(self.graph.edge_label(path[i],path[i+1]))
			else:
				self.graph.set_edge_label(path[i],path[i+1],self.colour_vertex[path[i]][0])
				del self.colour_vertex[path[i]][0]
			self.graphlst.append(self.graph.copy())
			self.pebs[path[i]]=self.pebs[path[i]]-1
			self.pebs[path[i+1]]=self.pebs[path[i+1]]+1	

	
	def path_from_dict(self,diction,u,lst):
		""" This recursive method is used to convert a dictionary
		with the vertices of the graph as keys, and the
		edges to be reversed as values to a list with
		just the path to the pebble. """
		if(u not in diction.keys()):
			lst.append(u)
			return lst
		else:
			lst.append(u)
			return self.path_from_dict(diction,diction[u][1],lst)

	
	def canonpath(self,u,w):
		""" This method initialises the search for the
		canonical path when wanting to an edge between
		vertices u and w in the coloured pebble game """
		C=self.graph.copy()
		dfs= list(C.depth_first_search(u))
		dfs.remove(u)           
		orig_path=[]
		for i in dfs:
			if(i==w):
				continue
			elif(self.pebs[i]!=0):
				orig_path=self.graph.shortest_path(u,i)
				break		
		orig_path=orig_path[::-1]
		color_vert=self.colour_vertex.copy()
		edges=self.graph.copy()
		Path =self.canon(color_vert,edges,orig_path)		
		O=Path.copy()
		Path.clear()
		KK=[]
		W=self.path_from_dict(O,u,KK)
		W.reverse()
		ES=self.edge_swap(W)
		self.graphlst.append(self.graph.copy())
		return ES	

	
	def canon(self,color_array,edge_color,path,v=0,canon_path={}):
		""" This method recursively looks for the correct canonical
		path to be reversed in the coloured pebble game so that the 
		reversing of an edge does not create a monochromatic cycle. """
		if(path[v]==path[-1]):
			q=[path[v],path[v-1]]
			Tr=edge_color.all_paths(q[0],q[1])
			L=self.monochrom1(color_array[path[v-1]],Tr,edge_color)
			if(L==False):
				canon_path.update({path[-1]:(path[-1],path[v-1])})
				return canon_path
			else:
				for i in range(len(L)-1):
					canon_path.update({L[i]:(L[i],L[i+1])})
				return canon_path
		elif(v==len(path)):
			return canon_path
		elif(len(color_array[path[v]]) !=0):
			return self.canon(color_array,edge_color,path,v+1,canon_path)
		else:
			if(edge_color.edge_label(path[v],path[v-1]) in color_array[path[v-1]]):
				canon_path.update({path[v]:(path[v],path[v-1])})
				color_array[v]+=[edge_color.edge_label(path[v],path[v-1])]
				return self.canon(color_array,edge_color,path,v+1,canon_path)
			else:
				P=edge_color.all_paths(path[v],path[v-1])
				M=self.monochrom1(color_array[path[v-1]],P,edge_color)
				if(M==False):
					canon_path.update({path[v]:(path[v],path[v-1])})
					color_array[v]+=[edge_color.edge_label(path[v],path[v-1])]
					return self.canon(color_array,edge_color,path,v+1,canon_path)
				M.reverse()
				return self.canon(color_array,edge_color,M,0,canon_path)


		""" These last four methods are the ones used to run the
		pebble games themselves. first the basic pebble game,
		then the pebble game variant I made which is called the
		weighted pebble game, then the component pebble game,
		and finally the canonical pebble game."""

	
	def PG(self):
		""" This is the main algorithm for the basic pebble game.
		Notice that the while loop ends when there is l pebbles
		left on D, or when each edge from the input graph G has 
		been accpeted or made redundant. """
		while(sum(self.pebs)!= self.l and len(self.G.edges())!= 0):		
			o=self.graph.random_vertex() 										
			if(len(self.G.neighbors(o))!=0):						 		
				j=sorted(self.G.neighbors(o))[0]						
															
			else:												
				continue

			if(self.pebs[o]+self.pebs[j]>=self.l+1):				
				self.add_edge1(o,j)						    			   
				self.G.delete_edge(o,j)						
				if(sum(self.pebs)==self.l and len(self.G.edges())!=0):
					self.red = self.red + len(self.G.edges())
				 						
			elif(self.pebs[o]+self.pebs[j]<self.l+1):            
				self.gather(o,j)						    
				while(self.pebs[o]+self.pebs[j]<self.l+1):			 
					if not self.gather(o,j):					 
						break
															 
				if(self.pebs[o]+self.pebs[j]>=self.l+1):		
					self.add_edge1(o,j)				
					self.G.delete_edge(o,j)				  
					if(sum(self.pebs)==self.l and len(self.G.edges())!=0):
						self.red = self.red + len(self.G.edges())						
				else:					
					self.red = self.red +1 						  
					self.G.delete_edge(o,j) 			 
					continue
		return True

	
	def PGweight(self):
		""" This is the main algortihm for the weighted pebble
		game variant I made. Instead of randomly chosing an 
		edge to add, it goes through the list of edge weights
		which was sorted in the initialising function accepting
		them or rejecting them until the list is empty or number
		of pebbles on D is equal to l. """
		while(len(self.weight)!=0 or (sum(self.pebs)!=self.l)):		
			o=self.weight[0][0][0]										
			j=self.weight[0][0][1]						
			if(self.pebs[o]+self.pebs[j]>=self.l+1):				
				self.add_edge1(o,j)
				self.graph.set_edge_label(i,j,self.weight[0][1])
				self.graphlst.append(self.graph.copy())	
				self.G.delete_edge(o,j)					 
				del self.weight[0]
				if(sum(self.pebs)==self.l and len(self.G.edges())!=0):
					self.red = self.red + len(self.G.edges())						
	
			elif(self.pebs[o]+self.pebs[j]<self.l+1):            
				self.gather(o,j)						    
				while((self.pebs[o]+self.pebs[j])<self.l+1):		
					if not self.gather(o,j):
						break
					
				if(self.pebs[o]+self.pebs[j]>=self.l+1):		
					self.add_edge1(o,j)
					self.graph.set_edge_label(i,j,self.weight[0][1])
					self.graphlst.append(self.graph.copy())				
					self.G.delete_edge(o,j)
					del self.weight[0]				  
					if(sum(self.pebs)==self.l and len(self.G.edges())!=0):
						self.red = self.red + len(self.G.edges())						
				else:					
					self.red = self.red +1 
					del self.weight[0]
					self.G.delete_edge(o,j) 			
					continue
						
		return True

	
	def PGcomp(self):
		""" This is the main algorithm for the components pebble game.
		It checks that two vertices in D aren't already in a common
		component before adding an edge between them. It does this
		by referencing the component matrix self.compindex. """
		while((sum(self.pebs)!= self.l) and (len(self.G.edges())!= 0)):		
			o=self.graph.random_vertex() 										
			if(len(self.G.neighbors(o))!=0):						 		
				j=sorted(self.G.neighbors(o))[0]							
			else:												
				continue
			if((self.pebs[o]+self.pebs[j]>=self.l+1) and (self.compindex[o][j]!=1)):				
				self.add_edge1(o,j)						 			   
				self.G.delete_edge(o,j)						
				self.make_comp(o,j)
				self.graphlst.append(self.graph.copy().plot(edge_colors=self.colour_edge, vertex_labels=False))
				self.colour_edge.clear()					
	
			elif((self.pebs[o]+self.pebs[j]<self.l+1) and (self.compindex[o][j]!=1)):            
				self.gather(o,j)						     
				while(self.pebs[o]+self.pebs[j]<self.l+1): 	 
					if not self.gather(o,j):					 
						break									 
												
				if(self.pebs[o]+self.pebs[j]>=self.l+1):		
					self.add_edge1(o,j)				
					self.G.delete_edge(o,j)
					self.make_comp(o,j)				  
					self.graphlst.append(self.graph.copy().plot(edge_colors=self.colour_edge, vertex_labels=False))
					self.colour_edge.clear()
			
			elif(self.compindex[o][j]==1):					
				self.red = self.red +1 						  
				self.G.delete_edge(o,j) 			
				continue
		return True		

	
	def PGcolour(self):
		""" This is the main algorithm used for the canonical 
		coloured pebble game to check for sparsity and to
		find k edge-disjoint spanning trees in a graph 
		which is (k,k)-tight. """
		while((sum(self.pebs)!= self.l) and (len(self.G.edges())!= 0)):		
			o=self.graph.random_vertex() 										
			if(len(self.G.neighbors(o))!=0):						 		
				j=sorted(self.G.neighbors(o))[0]							
			else:												
				continue

			t=set(self.colour_vertex[o])
			v=set(self.colour_vertex[j])
			a=list(t&v)
			if(self.pebs[o]+self.pebs[j]>=self.l+1):				
				if(t<=v or v<=t):
					self.add_edge1(o,j)						
					self.colour_vertex[o].remove(a[0])	
					self.graph.set_edge_label(o,j,a[0])
					self.G.delete_edge(o,j)	
					self.graphlst.append(self.graph.copy())					 			   
					if(sum(self.pebs)==self.l and len(self.G.edges())!=0):
						self.red = self.red + len(self.G.edges())
			elif(self.pebs[o]+self.pebs[j]<self.l+1):            						     
				while(self.pebs[o]+self.pebs[j]<self.l+1):
					if(self.pebs[o]<=self.pebs[j]):
						self.canonpath(o,j)
					else:
						self.canonpath(j,o)
								 
				q=set(self.colour_vertex[o])
				w=set(self.colour_vertex[j])
				e=list(q&w)

				if(self.pebs[o]+self.pebs[j]>=self.l+1):		
					if(q<=w or w<=q):
						self.add_edge1(o,j)
						self.colour_vertex[o].remove(e[0])	
						self.graph.set_edge_label(o,j,e[0])
						self.G.delete_edge(o,j)	
						self.graphlst.append(self.graph.copy())
						if(sum(self.pebs)==self.l and len(self.G.edges())!=0):
							self.red = self.red + len(self.G.edges())											  
				else:
					self.red = self.red +1 						  
					self.G.delete_edge(o,j) 			
					continue
		return True	
