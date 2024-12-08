#include <cstdint>
#include <unordered_map>
#include <iostream>
#include <string>
#include <sstream>
#include <fstream>
#include <stdio.h>
#include <ctype.h>
#include <cstring>
#include <limits.h>
#include <limits>
#include <cstddef>
#include <thread>
#include <SDL2/SDL.h>

#define CMD_BEGIN(cmd_class) \
	class cmd_class : public cmd::CmdBase { \
	public: \
		bool Run(cmd::Params params)

#define CMD_END(cmd_class, param_count, doc, halt_on_fail) \
}; \
static bool run_##cmd_class(cmd::Params params) { return cmd_class().Run(params); } \
const static bool cmd_class##_fn_added = cmd::Register(#cmd_class, run_##cmd_class, param_count, doc, halt_on_fail);


namespace cmd
{
	class Param
	{
	public:
		template < typename type_t >
		struct Parse
		{
			type_t value;
			bool   result;
		};

	private:
		const char *m_param;

	public:
		explicit Param(const char *param);
		operator const char*( void ) const;
		Parse<int64_t> Int( void ) const;
		Parse<double> Real( void ) const;
		Parse<bool> Bool( void ) const;
	};

	class Params
	{
	private:
		char     **m_params;
		uint32_t   m_param_count;
	
	public:
		Params(char **params, uint32_t param_count);
		uint32_t GetCount( void ) const;
		Param operator[](uint32_t i) const;
	};

	class CmdBase
	{
	public:
		virtual bool Run(const Params &params);
	};

	void Init(const char *app_name, const char *version);
	bool Register(const char *cmd_name, bool (*fn)(Params), uint32_t param_count, const char *doc, bool halt_on_fail = false);
	int Process(int argc, char **argv, bool halt_on_unrecognized = false);
}

struct Cmd
{
	bool        (*fn)(cmd::Params);
	std::string doc;
	uint32_t    param_count;
	bool        halt_on_fail;
};

static std::unordered_map<std::string, Cmd> &Cmds( void )
{
	static std::unordered_map<std::string, Cmd> cmds;
	return cmds;
}

static Cmd *Command(const std::string &cmd_name)
{
	auto cmd = Cmds().find(cmd_name);
	return cmd != Cmds().end() ? &((*cmd).second) : nullptr;
}

struct Info
{
	std::string app_name;
	std::string version;
	uint64_t    longest_cmd;
};

static Info &Information( void )
{
	static Info info = { "", "", 0 };
	return info;
}

cmd::Param::Param(const char *param) : m_param(param)
{}

cmd::Param::operator const char*( void ) const
{
	return m_param;
}

cmd::Param::Parse<int64_t> cmd::Param::Int( void ) const
{
	Parse<int64_t> p;
	std::istringstream sin;
	sin.str(m_param);
	p.result = bool(sin >> p.value);
	return p;
}

cmd::Param::Parse<double> cmd::Param::Real( void ) const
{
	Parse<double> p;
	std::istringstream sin;
	sin.str(m_param);
	p.result = bool(sin >> p.value);
	return p;
}

cmd::Param::Parse<bool> cmd::Param::Bool( void ) const
{
	Parse<bool> p;
	std::istringstream sin;
	sin.str(m_param);
	p.result = bool(sin >> p.value);
	return p;
}

cmd::Params::Params(char **params, uint32_t param_count) : m_params(params), m_param_count(param_count)
{}

uint32_t cmd::Params::GetCount( void ) const
{
	return m_param_count;
}

cmd::Param cmd::Params::operator[](uint32_t i) const
{
	return i < m_param_count ? Param(m_params[i]) : Param(nullptr);
}

bool cmd::CmdBase::Run(const cmd::Params&)
{
	return true;
}

void cmd::Init(const char *app_name, const char *version)
{
	Information().app_name = app_name;
	Information().version  = version;
}

bool cmd::Register(const char *cmd_name, bool (*fn)(cmd::Params), uint32_t param_count, const char *doc, bool halt_on_fail)
{
	std::string cmd = cmd_name;
	for (size_t i = 0; i < cmd.size(); ++i) {
		if (!((cmd[i] >= 'a' && cmd[i] <= 'z') || (cmd[i] >= 'A' && cmd[i] <= 'Z') || (cmd[i] >= '0' && cmd[i] <= '9'))) {
			cmd[i] = '-';
		}
	}
	Cmds()[cmd] = Cmd{ fn, doc, param_count, halt_on_fail };
	Information().longest_cmd = cmd.size() + 3 > Information().longest_cmd ? cmd.size() + 3 : Information().longest_cmd;
	return true;
}

int cmd::Process(int argc, char **argv, bool halt_on_unrecognized)
{
	bool success = true;
	for (int i = 1; i < argc; ++i) {
		Cmd *a = Command(argv[i]);
		if (a == nullptr) {
			std::cout << "unrecognized command: " << argv[i] << std::endl;
			if (halt_on_unrecognized) {
				return 1;
			}
		} else {
				if (i + a->param_count >= argc) {
				std::cout << "too few parameters: " << argv[i] << std::endl;
			} else if (!a->fn(Params(a->param_count > 0 ? argv + i + 1 : nullptr, a->param_count))) {
				success = false;
				if (a->halt_on_fail) { return 1; }
			}
			i += a->param_count;
		}
	}
	return success ? 0 : 1;
}

CMD_BEGIN(version)
{
	std::cout << Information().app_name << " " << Information().version << std::endl;
	return true;
}
CMD_END(version, 0, "Print version.", false)

CMD_BEGIN(help)
{
	version().Run(cmd::Params(nullptr, 0));
	for (auto i = Cmds().begin(); i != Cmds().end(); ++i) {
		std::cout << i->first;
		for (uint64_t j = 0; j < Information().longest_cmd - i->first.size(); ++j) {
			std::cout << " ";
		}
		std::cout << i->second.doc << std::endl;
	}
	return true;
}
CMD_END(help, 0, "Print help.", false)

#define JOBS_DERIVE(job_name, base_type) \
	class job_name; \
	template <> struct jobs_internal::rtti::type_info<job_name> { static const char *name( void ) { return #job_name; } }; \
	class job_name : public jobs_internal::inherit<job_name, jobs_internal::rtti::type_info<job_name>, base_type>

#define JOBS_NEW(job_name) \
	JOBS_DERIVE(job_name, job)

class job;

namespace jobs_internal
{
	uint64_t new_uuid( void );

	class rtti
	{
	protected:
		virtual void *self(uint64_t type_id);
		virtual const void *self(uint64_t type_id) const;

	public:
		virtual ~rtti( void );
		static uint64_t type_id( void );
		template < typename type_t > type_t *cast( void );
		template < typename type_t > const type_t *cast( void ) const;
		static rtti *instance( void );
		virtual uint64_t object_id( void ) const;
		virtual const char *object_name( void ) const;
		static const char *type_name( void );
		template < typename type_t > struct type_info {};
	};

	template < typename type_t, typename key_t = const char* >
	class search_tree
	{
	private:
		struct node
		{
			uint64_t  hash;
			key_t     key;
			node     *lte;
			node     *gt;
			type_t    value;
		};

	private:
		node *m_root;
	
	private:
		template < typename k_t > uint64_t make_hash(const k_t &s) const;
		uint64_t make_hash(const char *s) const;
		template < typename k_t > bool kcmp(const k_t &a, const k_t &b) const;
		bool kcmp(const char *a, const char *b) const;
		void free_node(node *n);
		template < typename fn_t > void traverse(fn_t &fn, node *n);

	public:
		search_tree( void );
		~search_tree( void );
		type_t *add(const key_t &key, const type_t &value);
		type_t *get(const key_t &key);
		const type_t *get(const key_t &key) const;
		void remove(const key_t &key);
		template < typename fn_t > void traverse(fn_t &fn);
	};

	template < typename self_t, typename self_type_name_t, typename base_t = job >
	class inherit : public base_t
	{
	private:
		static const bool m_registered;

	protected:
		void *self(uint64_t type_id);
		const void *self(uint64_t type_id) const;
	
	public:
		static uint64_t type_id( void );
		static jobs_internal::rtti *instance( void );
		uint64_t object_id( void ) const;
		const char *object_name( void ) const;
		static const char *type_name( void );
		static bool is_registered( void );
	};

	typedef rtti* (*instance_fn)(void);
}

JOBS_DERIVE(job, jobs_internal::rtti)
{
private:
	struct shared
	{
		uint64_t watchers;
		bool     deleted;
	};

	class base_callback
	{
	public:
		virtual ~base_callback( void ) {}
		virtual void operator()(job &sender) = 0;
	};

	template < typename job_t >
	class mem_callback : public base_callback
	{
	private:
		job_t *m_self;
		void (job_t::*m_memfn)(job&);

	public:
		mem_callback(job_t *self, void (job_t::*fn)(job&));
		void operator()(job &sender);
	};

	class fn_callback : public base_callback
	{
	private:
		void (*m_fn)(job&);
	
	public:
		fn_callback(void (*fn)(job&));
		void operator()(job &sender);
	};

	class callback
	{
	private:
		base_callback *m_callback;
	
	public:
		callback( void );
		~callback( void );
		template < typename job_t >
		void set(job_t *self, void (job_t::*fn)(job&));
		void set(void (*fn)(job&));
		void operator()(job &sender);
	};

	typedef jobs_internal::search_tree<callback> callback_tree;
	typedef jobs_internal::search_tree<callback_tree, uint64_t> event_tree;

public:
	template < typename job_t = job >
	class ref
	{
		friend class job;

	private:
		job_t  *m_job;
		shared *m_shared;
	
	public:
		ref( void );
		template < typename job2_t >
		explicit ref(job2_t *p = nullptr);
		template < typename job2_t >
		ref(const ref<job2_t> &r);
		template < typename job2_t > ref(ref<job2_t> &&r);
		~ref( void );
		template < typename job2_t > ref &operator=(const ref<job2_t> &r);
		template < typename job2_t > ref &operator=(ref<job2_t> &&r);
		template < typename job2_t > ref &operator=(job2_t *r);
		template < typename job2_t > void set_ref(job2_t *p);
		void release( void );
		job_t *get_job( void );
		const job_t *get_job( void ) const;
		template < typename job2_t > ref<job2_t> cast( void );
		job_t *operator->( void );
		const job_t *operator->( void ) const;
	};

	class query
	{
	public:
		class results;
		
		class result
		{
			friend class results;

		private:
			job::ref<>   m_job;
			result      *m_next;
			result     **m_prev;
		
		public:
			result(job &j);
			~result( void );
			job::ref<> &get_job( void );
			const job::ref<> &get_job( void ) const;
			result *get_next( void );
			const result *get_next( void ) const;
			result *remove( void );
		};

		class results
		{
		private:
			struct join_node
			{
				job     *value;
				int64_t  count;
			};

		private:
			static void insert_and_increment(jobs_internal::search_tree<join_node,const job*> &t, results &r);
			static void remove_and_decrement(jobs_internal::search_tree<join_node,const job*> &t, results &r);

		private:
			result  *m_first;
			result **m_end;
		
		public:
			results( void );
			~results( void );
			results(results &&r);
			results &operator=(results &&r);
			result *get_results( void );
			const result *get_results( void ) const;
			uint64_t count_results( void ) const;
			void add_result(job &j);
			template < typename query_t > results filter_results(const query_t &q);
			template < typename query_t > results filter_results( void );
			results filter_results(const query &q);

		public:
			static results join_and(results &a, results &b);
			static results join_or(results &a, results &b);
			static results join_sub(results &l, results &r);
			static results join_xor(results &a, results &b);
		};

	public:
		virtual ~query( void );
		virtual bool operator()(const job &j) const;
	};

private:
	static jobs_internal::search_tree<jobs_internal::instance_fn> m_products;

private:
	job                                  *m_parent;
	job                                  *m_sibling;
	job                                  *m_child;
	uint64_t                              m_job_id;
	uint64_t                              m_sleep_ns;
	uint64_t                              m_created_at_ns;
	uint64_t                              m_existed_for_ns;
	uint64_t                              m_active_for_ns;
	uint64_t                              m_existed_tick_count;
	uint64_t                              m_active_tick_count;
	uint64_t                              m_time_scale;
	uint64_t                              m_min_duration_ns;
	uint64_t                              m_max_duration_ns;
	uint64_t                              m_accumulated_duration_ns;
	uint64_t                              m_max_ticks_per_cycle;
	event_tree                            m_event_callbacks;
	shared                               *m_shared;
	bool                                  m_enabled;
	bool                                  m_kill;
	bool                                  m_waiting;
	bool                                  m_tick_lock;

private:
	void set_deleted( void );
	void add_sibling(job *&loc, job *p);
	void delete_siblings(job *&siblings);
	void delete_children(job *&children);
	void delete_killed_children(job *&child);
	void tick_children(uint64_t duration_ns);
	void get_notified(const char *event, job &sender);
	static uint64_t scale_time(uint64_t time, uint64_t time_scale);
	uint64_t get_parent_time_scale( void ) const;

protected:
	virtual void on_tick(uint64_t duration_ns);
	virtual void on_tock(uint64_t duration_ns);
	virtual void on_birth( void );
	virtual void on_death( void );

public:
	job( void );
	~job( void );
	void cycle(uint64_t duration_ns);
	void kill( void );
	void kill_children( void );
	void sleep_for(uint64_t duration_ns);
	void wake( void );
	template < typename job_t > void listen(const char *event, void (job_t::*callback)(job&));
	template < typename job_t > void listen(const char *event, const job &sender, void (job_t::*callback)(job&));
	void ignore(const char *event);
	void ignore(const char *event, const job &sender);
	void ignore(const job &sender);
	template < typename job_t > job_t *add_child( void );
	job *add_child(const char *type_name);
	void enable( void );
	void disable( void );
	bool is_killed( void ) const;
	bool is_alive( void ) const;
	bool is_enabled( void ) const;
	bool is_disabled( void ) const;
	bool is_sleeping( void ) const;
	bool is_awake( void ) const;
	bool is_active( void ) const;
	bool is_inactive( void ) const;
	bool is_waiting( void ) const;
	bool is_ready( void ) const;
	uint64_t get_job_id( void ) const;
	void notify_parent(const char *event);
	void notify_children(const char *event);
	void notify_group(const char *event, job::query::results &group);
	void notify(const char *event, job &target);
	ref<> get_ref( void );
	uint64_t get_existed_for_ns( void ) const;
	uint64_t get_active_for_ns( void ) const;
	uint64_t get_existed_tick_count( void ) const;
	uint64_t get_active_tick_count( void ) const;
	job *get_parent( void );
	const job *get_parent( void ) const;
	job *get_child( void );
	const job *get_child( void ) const;
	job *get_sibling( void );
	const job *get_sibling( void ) const;
	job *get_root( void );
	const job *get_root( void ) const;
	void set_local_time_scale(float time_scale);
	float get_local_time_scale( void ) const;
	void set_global_time_scale(float time_scale);
	float get_global_time_scale( void ) const;
	uint64_t get_local_time_ns( void ) const;
	uint64_t get_created_at_ns( void ) const;
	query::results filter_children(const query &q);
	template < typename query_t > query::results filter_children(const query_t &q);
	template < typename query_t > query::results filter_children( void );
	query::results get_children( void );
	template < typename job_t > query::results get_children( void );
	template < typename job_t > static bool register_job(const char *type_name);
	uint64_t count_children( void ) const;
	uint64_t count_decendants( void ) const;
	void limit_tick_interval(uint64_t min_duration_ns, uint64_t max_duration_ns);
	void unlimit_tick_interval( void );
	void limit_tick_rate(uint64_t min_ticks_per_sec, uint64_t max_ticks_per_sec);
	void unlimit_tick_rate( void );
	uint64_t get_min_duration_ns( void ) const;
	uint64_t get_max_duration_ns( void ) const;
	uint64_t get_min_tick_per_sec( void ) const;
	uint64_t get_max_tick_per_sec( void ) const;
	bool is_tick_limited( void ) const;
	static job *create_orphan(const char *type_name);
	bool has_enabled_children( void ) const;
	uint64_t get_max_tick_per_cycle( void ) const;
	void set_max_tick_per_cycle(uint64_t max_ticks_per_cyle);
	template < typename job_t = job > void defer(void (job_t::*mem_fn)(job&), uint64_t delay_ns);
	void run(uint64_t fixed_duration_ns = 0);
};

namespace jobs_internal
{
	JOBS_NEW(defer)
	{
	private:
		uint64_t m_target_time_ns;
	
	protected:
		void on_tick(uint64_t);
	
	public:
		defer( void );
		void set_delay(uint64_t ns);
	};
}

template < typename type_t >
type_t *jobs_internal::rtti::cast( void )
{
	return reinterpret_cast<type_t*>(self(type_t::type_id()));
}

template < typename type_t >
const type_t *jobs_internal::rtti::cast( void ) const
{
	return reinterpret_cast<const type_t*>(self(type_t::type_id()));
}

template < typename type_t, typename key_t >
template < typename k_t >
uint64_t jobs_internal::search_tree<type_t,key_t>::make_hash(const k_t &k) const
{
	const uint8_t *K = reinterpret_cast<const uint8_t*>(&k);
	uint64_t sum = 0xcbf29ce484222325ULL;
	for (uint64_t i = 0; i < sizeof(k_t); ++i) {
		sum ^= uint64_t(K[i]);
		sum *= 0x100000001b3ULL;
	}
	return sum;
}

template < typename type_t, typename key_t >
uint64_t jobs_internal::search_tree<type_t,key_t>::make_hash(const char *s) const
{
	uint64_t sum = 0xcbf29ce484222325ULL;
	for (uint64_t i = 0; s[i] != 0; ++i) {
		sum ^= uint64_t(s[i]);
		sum *= 0x100000001b3ULL;
	}
	return sum;
}

template < typename type_t, typename key_t >
template < typename k_t >
bool jobs_internal::search_tree<type_t,key_t>::kcmp(const k_t &a, const k_t &b) const
{
	const uint8_t *A = reinterpret_cast<const uint8_t*>(&a);
	const uint8_t *B = reinterpret_cast<const uint8_t*>(&b);
	for (uint32_t i = 0; i < sizeof(k_t); ++i) {
		if (A[i] != B[i]) { return false; };
	}
	return true;
}

template < typename type_t, typename key_t >
bool jobs_internal::search_tree<type_t,key_t>::kcmp(const char *a, const char *b) const
{
	while (*a != 0) {
		if (*a != *b) { return false; }
		++a;
		++b;
	}
	return (*a == *b);
}

template < typename type_t, typename key_t >
void jobs_internal::search_tree<type_t,key_t>::free_node(jobs_internal::search_tree<type_t,key_t>::node *n)
{
	if (n != nullptr) {
		free_node(n->lte);
		free_node(n->gt);
		delete n;
	}
}

template < typename type_t, typename key_t >
template < typename fn_t >
void jobs_internal::search_tree<type_t,key_t>::traverse(fn_t &fn, jobs_internal::search_tree<type_t,key_t>::node *n)
{
	if (n != nullptr) {
		traverse(fn, n->lte);
		fn(n->value);
		traverse(fn, n->gt);
	}
}

template < typename type_t, typename key_t >
jobs_internal::search_tree<type_t,key_t>::search_tree( void ) : m_root(nullptr) 
{}

template < typename type_t, typename key_t >
jobs_internal::search_tree<type_t,key_t>::~search_tree( void )
{
	free_node(m_root);
}

template < typename type_t, typename key_t >
type_t *jobs_internal::search_tree<type_t,key_t>::add(const key_t &key, const type_t &value)
{
	const uint64_t hash = make_hash(key);
	node **n = &m_root;
	while (*n != nullptr) {
		if (hash <= (*n)->hash) {
			if (hash == (*n)->hash && kcmp(key, (*n)->key)) {
				return &((*n)->value);
			} else {
				n = &((*n)->lte);
			}
		} else {
			n = &((*n)->gt);
		}
	}
	*n = new node{ hash, key, nullptr, nullptr, value };
	return &((*n)->value);
}

template < typename type_t, typename key_t >
type_t *jobs_internal::search_tree<type_t,key_t>::get(const key_t &key)
{
	const uint64_t hash = make_hash(key);
	node *n = m_root;
	while (n != nullptr) {
		if (hash <= n->hash) {
			if (hash == n->hash && kcmp(key, n->key)) {
				return &(n->value);
			} else {
				n = n->lte;
			}
		} else {
			n = n->gt;
		}
	}
	return nullptr;
}

template < typename type_t, typename key_t >
const type_t *jobs_internal::search_tree<type_t,key_t>::get(const key_t &key) const
{
	const uint64_t hash = make_hash(key);
	const node *n = m_root;
	while (n != nullptr) {
		if (hash <= n->hash) {
			if (hash == n->hash && kcmp(key, n->key)) {
				return &(n->value);
			} else {
				n = n->lte;
			}
		} else {
			n = n->gt;
		}
	}
	return nullptr;
}

template < typename type_t, typename key_t >
void jobs_internal::search_tree<type_t,key_t>::remove(const key_t &key)
{	
	const uint64_t hash = make_hash(key);

	node *p = nullptr;
	node *n = m_root;
	uint64_t rel = 0;
	
	while (n != nullptr && n->hash != hash && !kcmp(n->key, key)) {
		p = n;
		if (hash <= n->hash) {
			n = n->lte;
			rel = 1;
		} else {
			n = n->gt;
			rel = 2;
		}
	}

	if (n != nullptr) {
		
		const uint64_t child_count = (n->gt ? 1 : 0) + (n->lte ? 1 : 0);

		if (child_count == 0) {
			if (rel == 1)      { p->lte = nullptr; }
			else if (rel == 2) { p->gt  = nullptr; }
			else               { m_root = nullptr; }
		} else if (child_count == 1) {
			node *&c = n->gt ? n->gt : n->lte;
			if (rel == 1)      { p->lte = c; }
			else if (rel == 2) { p->gt  = c; }
			else               { m_root = c; }
			c = nullptr;
		} else if (child_count == 2) {
			node *c = n->gt;
			while (c->lte != nullptr) {
				c = c->lte;
			}
			if (rel == 1)      { p->lte = c; }
			else if (rel == 2) { p->gt  = c; }
			else               { m_root = c; }
			c->lte = n->lte;
			c->gt  = n->gt;
			n->lte = nullptr;
			n->gt  = nullptr;
		}
		delete n;
	}
}

template < typename type_t, typename key_t >
template < typename fn_t >
void jobs_internal::search_tree<type_t,key_t>::traverse(fn_t &fn)
{
	traverse(fn, m_root);
}

template < typename self_t, typename self_type_name_t, typename base_t >
const bool jobs_internal::inherit<self_t,self_type_name_t,base_t>::m_registered = job::register_job<self_t>(jobs_internal::inherit<self_t,self_type_name_t,base_t>::type_name());

template < typename self_t, typename self_type_name_t, typename base_t >
void *jobs_internal::inherit<self_t,self_type_name_t,base_t>::self(uint64_t type_id)
{
	return this->type_id() == type_id ? this : base_t::self(type_id);
}

template < typename self_t, typename self_type_name_t, typename base_t >
const void *jobs_internal::inherit<self_t,self_type_name_t,base_t>::self(uint64_t type_id) const
{
	return this->type_id() == type_id ? this : base_t::self(type_id);
}

template < typename self_t, typename self_type_name_t, typename base_t >
uint64_t jobs_internal::inherit<self_t,self_type_name_t,base_t>::type_id( void )
{
	static const uint64_t id = jobs_internal::new_uuid();
	return id;
}

template < typename self_t, typename self_type_name_t, typename base_t >
jobs_internal::rtti *jobs_internal::inherit<self_t,self_type_name_t,base_t>::instance( void )
{
	return new self_t;
}

template < typename self_t, typename self_type_name_t, typename base_t >
uint64_t jobs_internal::inherit<self_t,self_type_name_t,base_t>::object_id( void ) const
{
	return type_id();
}

template < typename self_t, typename self_type_name_t, typename base_t >
const char *jobs_internal::inherit<self_t,self_type_name_t,base_t>::object_name( void ) const
{
	return type_name();
}

template < typename self_t, typename self_type_name_t, typename base_t >
const char *jobs_internal::inherit<self_t,self_type_name_t,base_t>::type_name( void )
{
	return self_type_name_t::name();
}

template < typename self_t, typename self_type_name_t, typename base_t >
bool jobs_internal::inherit<self_t,self_type_name_t,base_t>::is_registered( void )
{
	return m_registered;
}

template < typename job_t >
job::mem_callback<job_t>::mem_callback(job_t *self, void (job_t::*fn)(job&)) : m_self(self), m_memfn(fn)
{}

template < typename job_t >
void job::mem_callback<job_t>::operator()(job &sender)
{
	if (m_self != nullptr && m_memfn != nullptr) {
		(m_self->*m_memfn)(sender);
	}
}

template < typename job_t >
void job::callback::set(job_t *self, void (job_t::*fn)(job&))
{
	delete m_callback;
	m_callback = new mem_callback<job_t>(self, fn);
}

template < typename job_t >
job::ref<job_t>::ref( void ) : m_job(nullptr), m_shared(nullptr)
{}

template < typename job_t >
template < typename job2_t >
job::ref<job_t>::ref(job2_t *p) : m_job(p), m_shared(p != nullptr ? p->m_shared : nullptr)
{
	if (m_shared != nullptr) {
		++m_shared->watchers;
	}
}

template < typename job_t >
template < typename job2_t >
job::ref<job_t>::ref(const job::ref<job2_t> &r) : ref()
{
	set_ref(r.m_job);
}

template < typename job_t >
template < typename job2_t >
job::ref<job_t>::ref(job::ref<job2_t> &&r) : m_job(r.m_job), m_shared(r.m_shared)
{
	r.m_job = nullptr;
	r.m_shared = nullptr;
}

template < typename job_t >
job::ref<job_t>::~ref( void )
{
	release();
}

template < typename job_t >
template < typename job2_t >
job::ref<job_t> &job::ref<job_t>::operator=(const job::ref<job2_t> &r)
{
	if (this != &r) {
		set_ref(r.m_job);
	}
	return *this;
}

template < typename job_t >
template < typename job2_t >
job::ref<job_t> &job::ref<job_t>::operator=(job::ref<job2_t> &&r)
{
	if (this != &r) {
		set_ref(r.m_job);
		r.release();
	}
	return *this;
}

template < typename job_t >
template < typename job2_t >
job::ref<job_t> &job::ref<job_t>::operator=(job2_t *r)
{
	set_ref(r);
	return *this;
}

template < typename job_t >
template < typename job2_t >
void job::ref<job_t>::set_ref(job2_t *p)
{
	if (p != m_job) {
		release();
		if (p != nullptr) {
			m_job = p;
			m_shared = m_job->m_shared;
			++m_shared->watchers;
		}
	}
}

template < typename job_t >
void job::ref<job_t>::release( void )
{
	if (m_shared != nullptr) {
		--m_shared->watchers;
		if (m_shared->watchers == 0 && m_shared->deleted) {
			delete m_shared;
		}
	}
	m_job = nullptr;
	m_shared = nullptr;
}

template < typename job_t >
job_t *job::ref<job_t>::get_job( void )
{
	return m_shared != nullptr && !m_shared->deleted ? m_job : nullptr;
}

template < typename job_t >
const job_t *job::ref<job_t>::get_job( void ) const
{
	return m_shared != nullptr && !m_shared->deleted ? m_job : nullptr;
}

template < typename job_t >
template < typename job2_t >
job::ref<job2_t> job::ref<job_t>::cast( void )
{
	return ref<job2_t>(m_job->template cast<job2_t>());
}

template < typename job_t >
job_t *job::ref<job_t>::operator->( void )
{
	return m_job;
}

template < typename job_t >
const job_t *job::ref<job_t>::operator->( void ) const
{
	return m_job;
}

template < typename query_t >
job::query::results job::query::results::filter_results(const query_t &q)
{
	results r;
	result *c = get_results();
	while (c != nullptr) {
		if (q(*c->get_job().get_job())) {
			r.add_result(*c->get_job().get_job());
		}
		c = c->get_next();
	}
	return r;
}

template < typename query_t >
job::query::results job::query::results::filter_results( void )
{
	return filter_results<query_t>(query_t());
}

template < typename job_t >
void job::listen(const char *event, void (job_t::*fn)(job&))
{
	job_t *self = cast<job_t>();
	if (self != nullptr) {
		callback_tree *t = m_event_callbacks.get(0);
		if (t == nullptr) {
			t = m_event_callbacks.add(0, callback_tree());
		}
		callback *c = t->add(event, callback());
		c->set<job_t>(self, fn);
	}
}

template < typename job_t >
void job::listen(const char *event, const job &sender, void (job_t::*fn)(job&))
{
	job_t *self = cast<job_t>();
	if (self != nullptr) {
		callback_tree *t = m_event_callbacks.get(sender.get_job_id());
		if (t == nullptr) {
			t = m_event_callbacks.add(sender.get_job_id(), callback_tree());
		}
		callback *c = t->add(event, callback());
		c->set<job_t>(self, fn);
	}
}

template < typename job_t >
job_t *job::add_child( void )
{
	job_t *p = nullptr;
	if (!is_killed()) {
		p = new job_t;
		add_sibling(m_child, p);
		job *b = dynamic_cast<job*>(p);
		b->m_created_at_ns = get_local_time_ns();
		b->m_min_duration_ns = m_min_duration_ns;
		b->m_max_duration_ns = m_max_duration_ns;
		b->on_birth();
	}
	return p;
}

template < typename query_t >
job::query::results job::filter_children(const query_t &q)
{
	return get_children().filter_results<query_t>(q);
}

template < typename query_t >
job::query::results job::filter_children( void )
{
	return filter_children<query_t>(query_t());
}

template < typename job_t >
job::query::results job::get_children( void )
{
	class type_filter : public job::query { public: bool operator()(const job &j) const { return j.cast<job_t>() != nullptr; } } q;
	return filter_children(q);
}

template < typename job_t >
bool job::register_job(const char *type_name)
{
	if (m_products.get(type_name) != nullptr) {
		return false;
	}
	m_products.add(type_name, job_t::instance);
	return true;
}

template < typename job_t >
void job::defer(void (job_t::*mem_fn)(job&), uint64_t delay_ns)
{
	jobs_internal::defer *c = add_child<jobs_internal::defer>();
	c->set_delay(delay_ns);
	listen<job_t>("defer", *c, mem_fn);
}

#define NS_PER_SEC 1000000000ULL

uint64_t jobs_internal::new_uuid( void )
{
	static uint64_t uuid = 1;
	return uuid++;
}

void *jobs_internal::rtti::self(uint64_t type_id)
{
	return this->type_id() == type_id ? this : nullptr;
}

const void *jobs_internal::rtti::self(uint64_t type_id) const
{
	return this->type_id() == type_id ? this : nullptr;
}

jobs_internal::rtti::~rtti( void )
{}

uint64_t jobs_internal::rtti::type_id( void )
{
	static const uint64_t id = jobs_internal::new_uuid();
	return id;
}

jobs_internal::rtti *jobs_internal::rtti::instance( void )
{
	return new rtti;
}

uint64_t jobs_internal::rtti::object_id( void ) const
{
	return type_id();
}

const char *jobs_internal::rtti::object_name( void ) const
{
	return type_name();
}

const char *jobs_internal::rtti::type_name( void )
{
	return "rtti";
}

job::fn_callback::fn_callback(void (*fn)(job&)) : m_fn(fn)
{}

void job::fn_callback::operator()(job &sender)
{
	if (m_fn != nullptr) {
		m_fn(sender);
	}
}

job::callback::callback( void ) : m_callback(nullptr)
{}

job::callback::~callback( void )
{
	delete m_callback;
}

void job::callback::set(void (*fn)(job&))
{
	delete m_callback;
	m_callback = new fn_callback(fn);
}

void job::callback::operator()(job &sender)
{
	if (m_callback != nullptr) {
		(*m_callback)(sender);
	}
}

job::query::result::result(job &j) : m_job(j.get_ref()), m_next(nullptr)
{}

job::query::result::~result( void )
{
	delete m_next;
	m_next = nullptr;
}

job::ref<> &job::query::result::get_job( void )
{
	return m_job;
}

const job::ref<> &job::query::result::get_job( void ) const
{
	return m_job;
}

job::query::result *job::query::result::get_next( void )
{
	return m_next;
}

const job::query::result *job::query::result::get_next( void ) const
{
	return m_next;
}

job::query::result *job::query::result::remove( void )
{
	job::query::result *next = m_next;
	if (m_prev != nullptr) {
		*m_prev = m_next;
	}
	m_next = nullptr;
	delete this;
	return next;
}

void job::query::results::insert_and_increment(jobs_internal::search_tree<join_node,const job*> &t, job::query::results &r)
{
	result *i = r.get_results();
	while (i != nullptr) {
		join_node *n = t.get(i->get_job().get_job());
		if (n != nullptr) {
			++n->count;
		} else {
			t.add(i->get_job().get_job(), join_node{ i->get_job().get_job(), 1 });
		}
		i = i->get_next();
	}
}

void job::query::results::remove_and_decrement(jobs_internal::search_tree<join_node,const job*> &t, job::query::results &r)
{
	result *i = r.get_results();
	while (i != nullptr) {
		join_node *n = t.get(i->get_job().get_job());
		if (n != nullptr) {
			--n->count;
		}
		i = i->get_next();
	}
}

job::query::results::results( void ) : m_first(nullptr), m_end(&m_first)
{}

job::query::results::~results( void )
{
	delete m_first;
}

job::query::results::results(job::query::results &&r) : results()
{
	m_first = r.m_first;
	m_end = r.m_end;

	r.m_first = nullptr;
	r.m_end = &r.m_first;
}

job::query::results &job::query::results::operator=(job::query::results &&r)
{
	if (this != &r) {
		delete m_first;
		
		m_first = r.m_first;
		m_end   = r.m_end;
		
		r.m_first = nullptr;
		r.m_end = &r.m_first;
	}
	
	return *this;
}

job::query::result *job::query::results::get_results( void )
{
	return m_first;
}

const job::query::result *job::query::results::get_results( void ) const
{
	return m_first;
}

uint64_t job::query::results::count_results( void ) const
{
	const result *r = m_first;
	uint64_t c = 0;
	while (r != nullptr) {
		++c;
		r = r->get_next();
	}
	return c;
}

void job::query::results::add_result(job &j)
{
	*m_end = new query::result(j);
	m_end = &(*m_end)->m_next;
}

job::query::results job::query::results::filter_results(const job::query &q)
{
	results r;
	result *c = get_results();
	while (c != nullptr) {
		if (q(*c->get_job().get_job())) {
			r.add_result(*c->get_job().get_job());
		}
		c = c->get_next();
	}
	return r;
}

job::query::results job::query::results::join_and(job::query::results &a, job::query::results &b)
{
	jobs_internal::search_tree<join_node,const job*> t;
	insert_and_increment(t, a);
	insert_and_increment(t, b);
	results res;
	struct accum
	{
		results *r;
		accum(results *res) : r(res) {}
		void operator()(join_node &n) {
			if (n.count >= 2) {
				r->add_result(*n.value);
			}
		}
	} fn(&res);
	t.traverse(fn);
	return res;
}

job::query::results job::query::results::join_or(job::query::results &a, job::query::results &b)
{
	jobs_internal::search_tree<join_node,const job*> t;
	insert_and_increment(t, a);
	insert_and_increment(t, b);
	results res;
	struct accum
	{
		results *r;
		accum(results *res) : r(res) {}
		void operator()(join_node &n) {
			r->add_result(*n.value);
		}
	} fn(&res);
	t.traverse(fn);
	return res;
}

job::query::results job::query::results::join_sub(job::query::results &l, job::query::results &r)
{
	jobs_internal::search_tree<join_node,const job*> t;
	insert_and_increment(t, l);
	remove_and_decrement(t, r);
	results res;
	struct accum
	{
		results *r;
		accum(results *res) : r(res) {}
		void operator()(join_node &n) {
			if (n.count == 1) {
				r->add_result(*n.value);
			}
		}
	} fn(&res);
	t.traverse(fn);
	return res;
}

job::query::results job::query::results::join_xor(job::query::results &a, job::query::results &b)
{
	jobs_internal::search_tree<join_node,const job*> t;
	insert_and_increment(t, a);
	insert_and_increment(t, b);
	results res;
	struct accum
	{
		results *r;
		accum(results *res) : r(res) {}
		void operator()(join_node &n) {
			if (n.count == 1) {
				r->add_result(*n.value);
			}
		}
	} fn(&res);
	t.traverse(fn);
	return res;
}

job::query::~query( void )
{}

bool job::query::operator()(const job &j) const
{
	return true;
}

jobs_internal::search_tree<jobs_internal::instance_fn> job::m_products = jobs_internal::search_tree<jobs_internal::instance_fn>();

void job::set_deleted( void )
{
	m_shared->deleted = true;
}

void job::add_sibling(job *&loc, job *p)
{
	job *old_loc = loc;
	loc = p;
	loc->m_parent = this;
	loc->m_sibling = old_loc;
}

void job::delete_siblings(job *&siblings)
{
	if (siblings != nullptr) {
		delete_siblings(siblings->m_sibling);
		delete siblings;
		siblings = nullptr;
	}
}

void job::delete_children(job *&children)
{
	if (children != nullptr) {
		delete_siblings(children->m_sibling);
		delete children;
		children = nullptr;
	}
}

void job::delete_killed_children(job *&child)
{
	if (child != nullptr) {
		delete_killed_children(child->m_sibling);
		if (child->is_killed()) {
			job *sibling = child->m_sibling;
			child->m_sibling = nullptr;
			delete child;
			child = sibling;
		}
	}
}

void job::tick_children(uint64_t duration_ns)
{
	for (job *c = m_child; c != nullptr && is_active(); c = c->m_sibling) {
		c->cycle(duration_ns);
	}
}

void job::get_notified(const char *event, job &sender)
{
	if (is_active()) {
		callback_tree *t = m_event_callbacks.get(0);
		if (t != nullptr) {
			callback *c = t->get(event);
			if (c != nullptr) {
				(*c)(sender);
			}
		}
		t = m_event_callbacks.get(sender.get_job_id());
		if (t != nullptr) {
			callback *c = t->get(event);
			if (c != nullptr) {
				(*c)(sender);
			}
		}
	}
}

uint64_t job::scale_time(uint64_t time, uint64_t time_scale)
{
	return (time * time_scale) >> 16ULL;
}

uint64_t job::get_parent_time_scale( void ) const
{
	return m_parent != nullptr ? ((m_parent->m_time_scale * m_parent->get_parent_time_scale()) >> 16ULL) : (1ULL << 16ULL);
}

void job::on_tick(uint64_t duration)
{}

void job::on_tock(uint64_t duration)
{}

void job::on_birth( void )
{}

void job::on_death( void )
{}

job::job( void ) :
	m_parent(nullptr), m_sibling(nullptr), m_child(nullptr),
	m_job_id(jobs_internal::new_uuid()),
	m_sleep_ns(0),
	m_created_at_ns(0),
	m_existed_for_ns(0), m_active_for_ns(0), m_existed_tick_count(0), m_active_tick_count(0),
	m_min_duration_ns(0), m_max_duration_ns(UINT64_MAX), m_accumulated_duration_ns(m_min_duration_ns), m_max_ticks_per_cycle(1),
	m_time_scale(1ULL << 16ULL),
	m_event_callbacks(),
	m_shared(new shared{ 0, false }),
	m_enabled(true), m_kill(false), m_waiting(false), m_tick_lock(false)
{}

job::~job( void )
{
	delete m_child;
	m_child = nullptr;
	delete m_sibling;
	m_sibling = nullptr;

	set_deleted();
	if (m_shared->watchers == 0) {
		delete m_shared;
		m_shared = nullptr;
	}
}

void job::cycle(uint64_t duration_ns)
{
	if (!m_tick_lock) {
		m_tick_lock = true;
		m_waiting = false;

		duration_ns = scale_time(duration_ns, m_time_scale);
		m_accumulated_duration_ns += duration_ns;

		const uint64_t min_dur_ns = m_min_duration_ns;
		const uint64_t max_dur_ns = m_max_duration_ns;

		for (uint64_t i = m_max_ticks_per_cycle; i > 0; --i) {

			duration_ns = m_accumulated_duration_ns > max_dur_ns ? max_dur_ns : m_accumulated_duration_ns;

			m_existed_for_ns += duration_ns;
			++m_existed_tick_count;

			if (is_sleeping()) {
				if (m_sleep_ns <= duration_ns) {
					m_sleep_ns = 0;
					duration_ns -= m_sleep_ns;
				} else {
					m_sleep_ns -= duration_ns;
					duration_ns = 0;
				}
			}

			if (duration_ns < min_dur_ns) {
				m_tick_lock = false;
				m_waiting = true;
				return;
			}
			m_accumulated_duration_ns -= duration_ns;

			if (is_active()) {
				m_active_for_ns += duration_ns;
				++m_active_tick_count;
				on_tick(duration_ns);
			}

			tick_children(duration_ns);

			delete_killed_children(m_child);

			if (is_active()) {
				on_tock(duration_ns);
			}
		}

		m_accumulated_duration_ns = max_dur_ns > 0 ? m_accumulated_duration_ns % max_dur_ns : 0;
		m_tick_lock = false;
	}
}

void job::kill( void )
{
	if (is_alive()) {
		kill_children();

		delete m_child;
		m_child = nullptr;

		on_death();

		m_enabled = false;
		m_kill    = true;
	}
}

void job::kill_children( void )
{
	for (job *c = m_child; c != nullptr; c = c->m_sibling) {
		c->kill();
	}
}

void job::sleep_for(uint64_t duration_ns)
{
	m_sleep_ns = m_sleep_ns > duration_ns ? m_sleep_ns : duration_ns;
}

void job::wake( void )
{
	m_sleep_ns = 0;
}

void job::ignore(const char *event)
{
	callback_tree *t = m_event_callbacks.get(0);
	if (t != nullptr) {
		t->remove(event);
	}
}

void job::ignore(const char *event, const job &sender)
{
	callback_tree *t = m_event_callbacks.get(sender.get_job_id());
	if (t != nullptr) {
		t->remove(event);
	}
}

void job::ignore(const job &sender)
{
	m_event_callbacks.remove(sender.get_job_id());
}

job *job::add_child(const char *type_name)
{
	job *p = nullptr;
	if (!is_killed()) {
		p = create_orphan(type_name);
		if (p != nullptr) {
			add_sibling(m_child, p);
			p->m_created_at_ns = get_local_time_ns();
			p->m_min_duration_ns = m_min_duration_ns;
			p->m_max_duration_ns = m_max_duration_ns;
			p->on_birth();
		}
	}
	return p;
}

void job::enable( void )
{
	m_enabled = true;
}

void job::disable( void )
{
	m_enabled = false;
}

bool job::is_killed( void ) const
{
	return m_kill;
}

bool job::is_alive( void ) const
{
	return !is_killed();
}

bool job::is_enabled( void ) const
{
	return !is_killed() && m_enabled;
}

bool job::is_disabled( void ) const
{
	return !is_enabled();
}

bool job::is_sleeping( void ) const
{
	return m_sleep_ns > 0;
}

bool job::is_awake( void ) const
{
	return !is_sleeping();
}

bool job::is_active( void ) const
{
	return is_enabled() && !is_sleeping();
}

bool job::is_inactive( void ) const
{
	return !is_active();
}

bool job::is_waiting( void ) const
{
	return m_waiting;
}

bool job::is_ready( void ) const
{
	return !is_waiting();
}

uint64_t job::get_job_id( void ) const
{
	return m_job_id;
}

void job::notify_parent(const char *event)
{
	if (m_parent != nullptr && is_active()) {
		notify(event, *m_parent);
	}
}

void job::notify_children(const char *event)
{
	for (job *c = m_child; c != nullptr && is_active(); c = c->m_sibling) {
		notify(event, *c);
	}
}

void job::notify_group(const char *event, job::query::results &group)
{
	query::result *r = group.get_results();
	while (r != nullptr && is_active()) {
		notify(event, *r->get_job().get_job());
		r = r->get_next();
	}
}

void job::notify(const char *event, job &target)
{
	target.get_notified(event, *this);
}

job::ref<> job::get_ref( void )
{
	return ref<job>(this);
}

uint64_t job::get_existed_for_ns( void ) const
{
	return m_existed_for_ns;
}

uint64_t job::get_active_for_ns( void ) const
{
	return m_active_for_ns;
}

uint64_t job::get_existed_tick_count( void ) const
{
	return m_existed_tick_count;
}

uint64_t job::get_active_tick_count( void ) const
{
	return m_active_tick_count;
}

job *job::get_parent( void )
{
	return m_parent;
}

const job *job::get_parent( void ) const
{
	return m_parent;
}

job *job::get_child( void )
{
	return m_child;
}

const job *job::get_child( void ) const
{
	return m_child;
}

job *job::get_sibling( void )
{
	return m_sibling;
}

const job *job::get_sibling( void ) const
{
	return m_sibling;
}

job *job::get_root( void )
{
	job *r = this;
	while (r->m_parent != nullptr) {
		r = r->m_parent;
	}
	return r;
}

const job *job::get_root( void ) const
{
	const job *r = this;
	while (r->m_parent != nullptr) {
		r = r->m_parent;
	}
	return r;
}

void job::set_local_time_scale(float time_scale)
{
	const uint64_t new_scale = uint64_t(time_scale * double(1ULL << 16ULL));
	m_time_scale = new_scale > 0 ? new_scale : 1;
}

float job::get_local_time_scale( void ) const
{
	return float(m_time_scale / double(1ULL << 16ULL));
}

void job::set_global_time_scale(float time_scale)
{
	const uint64_t new_scale = (uint64_t(time_scale * double(1ULL << 16ULL)) << 16ULL) / get_parent_time_scale();
	m_time_scale = new_scale > 0 ? new_scale : 1;
}

float job::get_global_time_scale( void ) const
{
	return float(
		((m_time_scale * get_parent_time_scale()) >> 16ULL) / double(1ULL << 16ULL)
	);
}

uint64_t job::get_local_time_ns( void ) const
{
	return m_created_at_ns + m_existed_for_ns;
}

uint64_t job::get_created_at_ns( void ) const
{
	return m_created_at_ns;
}

job::query::results job::filter_children(const job::query &q)
{
	return get_children().filter_results(q);
}

job::query::results job::get_children( void )
{
	query::results r;
	job *c = get_child();
	while (c != nullptr) {
		r.add_result(*c);
		c = c->get_sibling();
	}
	return r;
}

uint64_t job::count_children( void ) const
{
	const job *n = get_child();
	uint64_t c = 0;
	while (n != nullptr) {
		++c;
		n = n->get_sibling();
	}
	return c;
}

uint64_t job::count_decendants( void ) const
{
	const job *n = get_child();
	uint64_t c = 0;
	while (n != nullptr) {
		c += (n->count_decendants() + 1);
		n = n->get_sibling();
	}
	return c;
}

void job::limit_tick_interval(uint64_t min_duration_ns, uint64_t max_duration_ns)
{
	m_min_duration_ns = min_duration_ns < max_duration_ns ? min_duration_ns : max_duration_ns;
	m_max_duration_ns = min_duration_ns > max_duration_ns ? min_duration_ns : max_duration_ns;
}

void job::unlimit_tick_interval( void )
{
	m_min_duration_ns = 0;
	m_max_duration_ns = UINT64_MAX;
}

void job::limit_tick_rate(uint64_t min_ticks_per_sec, uint64_t max_ticks_per_sec)
{
	limit_tick_interval(NS_PER_SEC / max_ticks_per_sec, NS_PER_SEC / min_ticks_per_sec);
}

void job::unlimit_tick_rate( void )
{
	unlimit_tick_interval();
}

uint64_t job::get_min_duration_ns( void ) const
{
	return m_min_duration_ns;
}

uint64_t job::get_max_duration_ns( void ) const
{
	return m_max_duration_ns;
}

uint64_t job::get_min_tick_per_sec( void ) const
{
	return m_max_duration_ns > 0 ? NS_PER_SEC / m_max_duration_ns : UINT64_MAX;
}

uint64_t job::get_max_tick_per_sec( void ) const
{
	return m_min_duration_ns > 0 ? NS_PER_SEC / m_min_duration_ns : UINT64_MAX;
}

bool job::is_tick_limited( void ) const
{
	return m_min_duration_ns != 0 || m_max_duration_ns != 0;
}

job *job::create_orphan(const char *type_name)
{
	job *j = nullptr;
	jobs_internal::instance_fn *i = m_products.get(type_name);
	if (i != nullptr) {
		jobs_internal::rtti *r = (*i)();
		if (r != nullptr) {
			j = r->cast<job>();
			if (j == nullptr) {
				delete r;
			}
			return j;
		}
	}
	return j;
}

bool job::has_enabled_children( void ) const
{
	const job *c = get_child();
	while (c != nullptr && c->is_disabled()) {
		c = c->get_sibling();
	}
	return c != nullptr;
}

uint64_t job::get_max_tick_per_cycle( void ) const
{
	return m_max_ticks_per_cycle;
}

void job::set_max_tick_per_cycle(uint64_t max_ticks_per_cyle)
{
	m_max_ticks_per_cycle = max_ticks_per_cyle > 0 ? max_ticks_per_cyle : 1;
}

void job::run(uint64_t fixed_duration_ns)
{
	on_birth();

	uint64_t duration_ns = fixed_duration_ns > get_min_duration_ns() ? fixed_duration_ns : get_min_duration_ns();

	while (is_enabled()) {
		
		const uint64_t start_ns = fixed_duration_ns == 0 ? std::chrono::high_resolution_clock::now().time_since_epoch().count() : 0;

		cycle(duration_ns);

		duration_ns = fixed_duration_ns == 0 ? std::chrono::high_resolution_clock::now().time_since_epoch().count() - start_ns : fixed_duration_ns;

		if (duration_ns < get_min_duration_ns()) {
			std::this_thread::sleep_for(std::chrono::nanoseconds(get_min_duration_ns() - duration_ns));
			duration_ns = get_min_duration_ns();
		}
	}
}

void jobs_internal::defer::on_tick(uint64_t)
{
	if (get_active_for_ns() >= m_target_time_ns) {
		notify_parent("defer");
		kill();
	}
}

jobs_internal::defer::defer( void ) :
	m_target_time_ns(get_active_for_ns())
{}

void jobs_internal::defer::set_delay(uint64_t ns)
{
	m_target_time_ns = get_active_for_ns() + ns;
}

static std::string buffer_file(const std::string &filename)
{
	std::string buffer;
	std::ostringstream sout;
	std::ifstream file(filename, std::ios::binary);
	if (file.is_open()) {
		sout << file.rdbuf();
	} else {
		std::cout << "Could not open file \'" << filename << "\'" << std::endl;
	}
	return sout.str();
}
struct chars
{
	char str[32];
	struct view
	{
		const char *str;
		unsigned    len;
		unsigned    page;
	};
};

bool is_num(char c);
bool is_alpha(char c);
bool is_alnum(char c);
bool is_white(char c);
chars to_chars(const char *str, unsigned len);

struct token
{
	enum tokentype
	{
		NONE = 0,
		KEYWORD = 1<<12,
		ALIAS = 2<<12,
		OPERATOR = 3<<12,
		LITERAL = 5<<12,
		STOP = 6<<12,
			STOP_ERR,
			STOP_EOF,
		COMMENT = 7<<12,
		CHAR = 8<<12,
		
		TYPEMASK0 = 0xF000,
		TYPEMASK1 = 0xFF00,
		TYPEMASK2 = 0xFFF0
	};

	chars     text;
	unsigned  hash;
	tokentype type;
	unsigned  user_type;
	unsigned  head;
	unsigned  row;
	unsigned  col;
	unsigned  index;
	unsigned  (*hashfn)(const char*,unsigned);
};

struct ctokn
{
	enum tokentype
	{
		KEYWORD_TYPE = token::KEYWORD | (1<<8),
			KEYWORD_TYPE_VOID,
			KEYWORD_TYPE_SIGNED,
			KEYWORD_TYPE_INT = KEYWORD_TYPE_SIGNED,
			KEYWORD_TYPE_UNSIGNED,
			KEYWORD_TYPE_FLOAT,
		KEYWORD_CONTROL = token::KEYWORD | (2<<8),
			KEYWORD_CONTROL_RETURN,
			KEYWORD_CONTROL_IF,
			KEYWORD_CONTROL_ELSE,
			KEYWORD_CONTROL_WHILE,
		KEYWORD_INTRINSIC = token::KEYWORD | (3<<8),
			KEYWORD_INTRINSIC_ASM,

		ALIAS_VAR = token::ALIAS | (1<<8),
		ALIAS_FUNC = token::ALIAS | (2<<8),
		
		OPERATOR_ARITHMETIC = token::OPERATOR | (1<<8),
			OPERATOR_ARITHMETIC_ADD,
			OPERATOR_ARITHMETIC_SUB,
			OPERATOR_ARITHMETIC_MUL,
			OPERATOR_ARITHMETIC_DIV,
			OPERATOR_ARITHMETIC_MOD,
		OPERATOR_ASSIGNMENT = token::OPERATOR | (2<<8),
			OPERATOR_ASSIGNMENT_SET,
		OPERATOR_ENCLOSE = token::OPERATOR | (3<<8),
			OPERATOR_ENCLOSE_PARENTHESIS = OPERATOR_ENCLOSE | (1<<4),
				OPERATOR_ENCLOSE_PARENTHESIS_L,
				OPERATOR_ENCLOSE_PARENTHESIS_R,
			OPERATOR_ENCLOSE_BRACKET = OPERATOR_ENCLOSE | (2<<4),
				OPERATOR_ENCLOSE_BRACKET_L,
				OPERATOR_ENCLOSE_BRACKET_R,
			OPERATOR_ENCLOSE_BRACE = OPERATOR_ENCLOSE | (3<<4),
				OPERATOR_ENCLOSE_BRACE_L,
				OPERATOR_ENCLOSE_BRACE_R,
		OPERATOR_SEMICOLON = token::OPERATOR | 1,
		OPERATOR_COLON,
		OPERATOR_COMMA,

		LITERAL_INT = token::LITERAL | (1<<8),
	};
};

struct lexer
{
	chars::view code;
	unsigned    head;
	unsigned    row;
	unsigned    col;
	unsigned    index;
	unsigned    page;
	token       last;
	chars::view (*load_page)(unsigned);
};

token new_token(const char *chars, unsigned char_count, token::tokentype type, unsigned user_type, unsigned (*hashfn)(const char*,unsigned) = nullptr);
token new_keyword(const char *chars, unsigned char_count, unsigned user_type, unsigned (*hashfn)(const char*,unsigned) = nullptr);
token new_operator(const char *chars, unsigned char_count, unsigned user_type, unsigned (*hashfn)(const char*,unsigned) = nullptr);
token new_literal(const char *chars, unsigned char_count, unsigned user_type, unsigned (*hashfn)(const char*,unsigned) = nullptr);
token new_alias(const char *chars, unsigned char_count, unsigned user_type, unsigned (*hashfn)(const char*,unsigned) = nullptr);
token new_comment(const char *chars, unsigned char_count);
token new_eof( void );
token new_error(const char *chars, unsigned char_count);
lexer init_lexer(chars::view code);
token lex(lexer *l, const token *tokens, signed num_tokens);
token chlex(lexer *l);
signed count_tokens(token (*lex_fn)(lexer*), lexer l);

#define MAX_REGEXP_OBJECTS 30
#define MAX_CHAR_CLASS_LEN 40

enum { UNUSED, DOT, BEGIN, END, QUESTIONMARK, STAR, PLUS, CHAR, CHAR_CLASS, INV_CHAR_CLASS, DIGIT, NOT_DIGIT, ALPHA, NOT_ALPHA, WHITESPACE, NOT_WHITESPACE, /* BRANCH */ };

typedef struct regex_t
{
	unsigned char type;
	union
	{
		unsigned char  ch;
		unsigned char *ccl;
	} u;
} regex_t;

static signed   re_match(const char *pattern, const char *text, signed textlength, signed *matchlength);
static signed   re_matchp(regex_t *pattern, const char *text, signed textlength, signed *matchlength);
static regex_t *re_compile(const char *pattern);
static void     re_prsigned(regex_t *pattern);
static signed   matchpattern(regex_t *pattern, const char *text, signed textlength, signed *matchlength);
static signed   matchcharclass(char c, const char* str);
static signed   matchstar(regex_t p, regex_t *pattern, const char *text, signed textlength, signed *matchlength);
static signed   matchplus(regex_t p, regex_t *pattern, const char *text, signed textlength, signed *matchlength);
static signed   matchone(regex_t p, char c);
static signed   matchdigit(char c);
static signed   matchalpha(char c);
static signed   matchwhitespace(char c);
static signed   matchmetachar(char c, const char *str);
static signed   matchrange(char c, const char *str);
static signed   matchdot(char c);
static signed   ismetachar(char c);

static signed re_match(const char *pattern, const char *text, signed textlength, signed *matchlength)
{
	return re_matchp(re_compile(pattern), text, textlength, matchlength);
}

static signed re_matchp(regex_t *pattern, const char *text, signed textlength, signed *matchlength)
{
	*matchlength = 0;
	if (pattern != 0) {
		if (pattern[0].type == BEGIN) {
			return ((matchpattern(&pattern[1], text, textlength, matchlength)) ? 0 : -1);
		} else {
			signed idx = -1;

			do {
				idx += 1;

				if (matchpattern(pattern, text, textlength, matchlength)) {
					if (text[0] == '\0') {
						return -1;
					}

					return idx;
				}
			} while (*text++ != '\0' && --textlength > 0);
		}
	}
	return -1;
}

static regex_t* re_compile(const char *pattern)
{
	static regex_t re_compiled[MAX_REGEXP_OBJECTS];
	static unsigned char ccl_buf[MAX_CHAR_CLASS_LEN];
	signed ccl_bufidx = 1;

	char c;
	signed i = 0;
	signed j = 0;

	while (pattern[i] != '\0' && (j+1 < MAX_REGEXP_OBJECTS)) {
		c = pattern[i];

		switch (c) {
			case '^': { re_compiled[j].type = BEGIN;        } break;
			case '$': { re_compiled[j].type = END;          } break;
			case '.': { re_compiled[j].type = DOT;          } break;
			case '*': { re_compiled[j].type = STAR;         } break;
			case '+': { re_compiled[j].type = PLUS;         } break;
			case '?': { re_compiled[j].type = QUESTIONMARK; } break;
		
			case '\\':
			{
				if (pattern[i+1] != '\0') {
					i += 1;
					switch (pattern[i]) {
					case 'd': { re_compiled[j].type = DIGIT;          } break;
					case 'D': { re_compiled[j].type = NOT_DIGIT;      } break;
					case 'w': { re_compiled[j].type = ALPHA;          } break;
					case 'W': { re_compiled[j].type = NOT_ALPHA;      } break;
					case 's': { re_compiled[j].type = WHITESPACE;     } break;
					case 'S': { re_compiled[j].type = NOT_WHITESPACE; } break;

					default:
						{
							re_compiled[j].type = CHAR;
							re_compiled[j].u.ch = pattern[i];
						}
						break;
					}
				}
			} break;

			case '[':
			{
				signed buf_begin = ccl_bufidx;

				if (pattern[i+1] == '^') {
					re_compiled[j].type = INV_CHAR_CLASS;
					i += 1;
					if (pattern[i+1] == 0) {
						return 0;
					}
				} else {
					re_compiled[j].type = CHAR_CLASS;
				}

				while ((pattern[++i] != ']') && (pattern[i]   != '\0'))
				{
					if (pattern[i] == '\\') {
						if (ccl_bufidx >= MAX_CHAR_CLASS_LEN - 1) {
							return 0;
						}
						if (pattern[i+1] == 0) {
							return 0;
						}
						ccl_buf[ccl_bufidx++] = pattern[i++];
					} else if (ccl_bufidx >= MAX_CHAR_CLASS_LEN) {
						return 0;
					}
					ccl_buf[ccl_bufidx++] = pattern[i];
				}
				if (ccl_bufidx >= MAX_CHAR_CLASS_LEN) {
					return 0;
				}
				ccl_buf[ccl_bufidx++] = 0;
				re_compiled[j].u.ccl = &ccl_buf[buf_begin];
			} break;

			default:
			{
				re_compiled[j].type = CHAR;
				re_compiled[j].u.ch = c;
			} break;
		}
		if (pattern[i] == 0) {
			return 0;
		}

		i += 1;
		j += 1;
	}
	re_compiled[j].type = UNUSED;

	return (regex_t*)re_compiled;
}

static void re_prsigned(regex_t *pattern)
{
	const char *types[] = { "UNUSED", "DOT", "BEGIN", "END", "QUESTIONMARK", "STAR", "PLUS", "CHAR", "CHAR_CLASS", "INV_CHAR_CLASS", "DIGIT", "NOT_DIGIT", "ALPHA", "NOT_ALPHA", "WHITESPACE", "NOT_WHITESPACE", "BRANCH" };

	signed i;
	signed j;
	char c;
	for (i = 0; i < MAX_REGEXP_OBJECTS; ++i) {
		if (pattern[i].type == UNUSED) {
			break;
		}

		printf("type: %s", types[pattern[i].type]);
		if (pattern[i].type == CHAR_CLASS || pattern[i].type == INV_CHAR_CLASS) {
			printf(" [");
			for (j = 0; j < MAX_CHAR_CLASS_LEN; ++j) {
				c = pattern[i].u.ccl[j];
				if ((c == '\0') || (c == ']')) {
					break;
				}
				printf("%c", c);
			}
			printf("]");
		} else if (pattern[i].type == CHAR) {
			printf(" '%c'", pattern[i].u.ch);
		}
		printf("\n");
	}
}

static signed matchdigit(char c)
{
	return isdigit(c);
}
static signed matchalpha(char c)
{
	return isalpha(c);
}
static signed matchwhitespace(char c)
{
	return isspace(c);
}
static signed matchalphanum(char c)
{
	return ((c == '_') || matchalpha(c) || matchdigit(c));
}
static signed matchrange(char c, const char* str)
{
	return
	(
		(c != '-')        &&
		(str[0] != '\0')  &&
		(str[0] != '-')   &&
		(str[1] == '-')   &&
		(str[2] != '\0')  &&
		(
			(c >= str[0]) &&
			(c <= str[2])
		)
	);
}
static signed matchdot(char c)
{
	return c != '\n' && c != '\r';
}
static signed ismetachar(char c)
{
	return ((c == 's') || (c == 'S') || (c == 'w') || (c == 'W') || (c == 'd') || (c == 'D'));
}

static signed matchmetachar(char c, const char *str)
{
	switch (str[0]) {
		case 'd': return  matchdigit(c);
		case 'D': return !matchdigit(c);
		case 'w': return  matchalphanum(c);
		case 'W': return !matchalphanum(c);
		case 's': return  matchwhitespace(c);
		case 'S': return !matchwhitespace(c);
		default:  return (c == str[0]);
	}
}

static signed matchcharclass(char c, const char *str)
{
	do {
		if (matchrange(c, str)) {
			return 1;
		}
		else if (str[0] == '\\') {
			str += 1;
			if (matchmetachar(c, str)) {
				return 1;
			}
			else if ((c == str[0]) && !ismetachar(c)) {
				return 1;
			}
		}
		else if (c == str[0]) {
			if (c == '-') {
				return ((str[-1] == '\0') || (str[1] == '\0'));
			}
			else {
				return 1;
			}
		}
	} while (*str++ != '\0');

	return 0;
}

static signed matchone(regex_t p, char c)
{
	switch (p.type) {
		case DOT:            return  matchdot(c);
		case CHAR_CLASS:     return  matchcharclass(c, (const char*)p.u.ccl);
		case INV_CHAR_CLASS: return !matchcharclass(c, (const char*)p.u.ccl);
		case DIGIT:          return  matchdigit(c);
		case NOT_DIGIT:      return !matchdigit(c);
		case ALPHA:          return  matchalphanum(c);
		case NOT_ALPHA:      return !matchalphanum(c);
		case WHITESPACE:     return  matchwhitespace(c);
		case NOT_WHITESPACE: return !matchwhitespace(c);
		default:             return  (p.u.ch == c);
	}
}

static signed matchstar(regex_t p, regex_t *pattern, const char *text, signed textlength, signed *matchlength)
{
	signed prelen = *matchlength;
	const char *preposigned = text;
	while ((text[0] != '\0') && matchone(p, *text)) {
		text++;
		--textlength;
		(*matchlength)++;
	}
	while (text >= preposigned) {
		if (matchpattern(pattern, text--, textlength + 1, matchlength)) {
			return 1;
		}
		(*matchlength)--;
	}

	*matchlength = prelen;
	return 0;
}

static signed matchplus(regex_t p, regex_t *pattern, const char *text, signed textlength, signed *matchlength)
{
	const char *preposigned = text;
	while ((text[0] != '\0') && matchone(p, *text)) {
		text++;
		--textlength;
		(*matchlength)++;
	}
	while (text > preposigned) {
		if (matchpattern(pattern, text--, textlength + 1, matchlength)) {
			return 1;
		}
		(*matchlength)--;
	}
	return 0;
}

static signed matchquestion(regex_t p, regex_t *pattern, const char *text, signed textlength, signed *matchlength)
{
	if (p.type == UNUSED) {
		return 1;
	}
	if (matchpattern(pattern, text, textlength, matchlength)) {
		return 1;
	}
	if (*text && matchone(p, *text++)) {
		if (matchpattern(pattern, text, textlength - 1, matchlength)) {
			(*matchlength)++;
			return 1;
		}
	}
	return 0;
}

static signed matchpattern(regex_t *pattern, const char *text, signed textlength, signed *matchlength)
{
	signed pre = *matchlength;
	do {
		if ((pattern[0].type == UNUSED) || (pattern[1].type == QUESTIONMARK)) {
			return matchquestion(pattern[0], &pattern[2], text, textlength, matchlength);
		} else if (pattern[1].type == STAR) {
			return matchstar(pattern[0], &pattern[2], text, textlength, matchlength);
		} else if (pattern[1].type == PLUS) {
			return matchplus(pattern[0], &pattern[2], text, textlength, matchlength);
		} else if ((pattern[0].type == END) && pattern[1].type == UNUSED) {
			return (text[0] == '\0') || textlength == 1;
		}
		(*matchlength)++;
	} while ((text[0] != '\0') && matchone(*pattern++, *text++) && --textlength >= 0);

	*matchlength = pre;
	return 0;
}

static unsigned init_hash_ch( void )
{
	return unsigned(0xcbf29ce484222325ULL);
}

static unsigned chhash(unsigned h, char ch)
{
	h ^= unsigned(ch);
	h *= unsigned(0x100000001b3ULL);
	return h;
};

static unsigned strhash(const char *str, unsigned len)
{
	unsigned h = init_hash_ch();
	for (signed i = 0; i < len; ++i) {
		h = chhash(h, str[i]);
	}
	return h;
}

static unsigned numhash(const char *nums, unsigned len)
{
	unsigned h = 0;
	for (unsigned i = 0; i < len; ++i) {
		h = h  * 10 + nums[i] - '0';
	}
	return h;
}

static unsigned numhashch(const char *ch, unsigned len)
{
	unsigned h = 0;
	for (unsigned i = 0; i < len; ++i) {
		h = h + unsigned(ch[i]);
	}
	return h;
}

bool is_num(char c)
{
	return (c >= '0' && c <= '9');
}

bool is_alpha(char c)
{
	return
		(c >= 'a' && c <= 'z') ||
		(c >= 'A' && c <= 'Z') ||
		c == '_';
}

bool is_alnum(char c)
{
	return
		is_num(c) ||
		is_alpha(c);
}

bool is_white(char c)
{
	return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\0';
}

chars to_chars(const char *str, unsigned len)
{
	chars c;
	unsigned max = len < sizeof(c.str) - 1 ? len : sizeof(c.str) - 1;
	for (unsigned i = 0; i < max; ++i) {
		c.str[i] = str[i];
	}
	for (unsigned i = max; i < sizeof(c.str); ++i) {
		c.str[i] = 0;
	}
	return c;
}

token new_token(const char *chars, unsigned char_count, token::tokentype type, unsigned user_type, unsigned (*hashfn)(const char*,unsigned))
{
	token t;

	const unsigned size = sizeof(t.text.str) - 1 < char_count ? sizeof(t.text.str) - 1 : char_count;
	for (unsigned i = 0; i < size; ++i)                  { t.text.str[i] = chars[i]; }
	for (unsigned i = size; i < sizeof(t.text.str); ++i) { t.text.str[i] = '\0'; }
	t.hashfn = hashfn;
	if (hashfn != nullptr) {
		t.hash = hashfn(chars, char_count);
	} else {
		t.hash = (type == token::LITERAL) ? numhash(chars, char_count) : strhash(chars, char_count);
	}
	t.type = type;
	t.user_type = user_type;
	t.head = t.row = t.col = t.index = 0;
	return t;
}

token new_keyword(const char *chars, unsigned char_count, unsigned user_type, unsigned (*hashfn)(const char*,unsigned))
{
	return new_token(chars, char_count, token::KEYWORD, user_type, hashfn);
}

token new_operator(const char *chars, unsigned char_count, unsigned user_type, unsigned (*hashfn)(const char*,unsigned))
{
	return new_token(chars, char_count, token::OPERATOR, user_type, hashfn);
}

token new_literal(const char *chars, unsigned char_count, unsigned user_type, unsigned (*hashfn)(const char*,unsigned))
{
	return new_token(chars, char_count, token::LITERAL, user_type, hashfn);
}

token new_alias(const char *chars, unsigned char_count, unsigned user_type, unsigned (*hashfn)(const char*,unsigned))
{
	return new_token(chars, char_count, token::ALIAS, user_type, hashfn);
}

token new_comment(const char *chars, unsigned char_count)
{
	return new_token(chars, char_count, token::COMMENT, 0, nullptr);
}

token new_eof( void )
{
	return new_token("", 0, token::STOP, token::STOP_EOF);
}

token new_error(const char *chars, unsigned char_count)
{
	return new_token(chars, char_count, token::STOP, token::STOP_ERR);
}

static bool scmp(chars::view a, chars::view b)
{
	if (a.len != b.len) { return false; }
	for (signed i = 0; i < a.len; ++i) {
		if (a.str[i] != b.str[i]) { return false; }
	}
	return true;
}

static void next_char(lexer *p)
{
	if (p->head < p->code.len) {
		++p->col;
		switch (p->code.str[p->head]) {
		case '\r':
		case '\n':
			p->col = 0;
			++p->row;
			break;
		}
		++p->head;
		if (p->head >= p->code.len && p->load_page != NULL) {
			p->code = p->load_page(++p->page);
		}
	}
}

static void skip_white(lexer *p)
{
	while (p->head < p->code.len && is_white(p->code.str[p->head])) {
		next_char(p);
	}
}

static token match_token(chars::view s, token::tokentype type, const token *tokens, signed num_tokens)
{
	unsigned h = strhash(s.str, s.len);
	for (signed i = 0; i < num_tokens; ++i) {
		if (tokens[i].type == type && tokens[i].hash == h) {
			return tokens[i];
		}
	}
	return new_error(s.str, s.len);
}

static token get_key(chars::view s, const token *tokens, signed num_tokens)
{
	return match_token(s, token::KEYWORD, tokens, num_tokens);
}

static token get_op(chars::view s, const token *tokens, signed num_tokens)
{
	return match_token(s, token::OPERATOR, tokens, num_tokens);
}

static token get_lit(chars::view s, const token *tokens, signed num_tokens)
{
	for (signed i = 0; i < num_tokens; ++i) {
		signed matchlen;
		if (tokens[i].type == token::LITERAL && re_match(tokens[i].text.str, s.str, s.len, &matchlen) == 0 && matchlen == s.len) {
			return new_literal(s.str, s.len, tokens[i].user_type, tokens[i].hashfn);
		}
	}
	return new_error(s.str, s.len);
}

static token get_alias(chars::view s, const token *tokens, signed num_tokens)
{
	for (signed i = 0; i < num_tokens; ++i) {
		signed matchlen;
		if (tokens[i].type == token::ALIAS && re_match(tokens[i].text.str, s.str, s.len, &matchlen) == 0 && matchlen == s.len) {
			return new_alias(s.str, s.len, tokens[i].user_type, tokens[i].hashfn);
		}
	}
	return new_error(s.str, s.len);
}

static token get_eof(chars::view s, const token *tokens, signed num_tokens)
{
	if (s.len > 0) {
		return new_error(s.str, s.len);
	}
	token t;
	t.hash = strhash(s.str, s.len);
	for (int i = 0; i < sizeof(chars::str); ++i) {
		t.text.str[i] = '\0';
	}
	t.type = token::STOP;
	t.user_type = token::STOP_EOF;
	t.index = t.row = t.col = t.head = 0;
	return t;
}

static token get_cmt(chars::view s, const token *tokens, signed num_tokens)
{
	return match_token(s, token::COMMENT, tokens, num_tokens);
}

static chars::view read_specials(lexer *p, unsigned &head, unsigned &row, unsigned &col, unsigned &index)
{
	char c;
	unsigned s = p->head;
	unsigned i = 0;
	while (i < sizeof(chars::str) - 1 && p->head < p->code.len) {
		c = p->code.str[p->head];
		if (is_alnum(c) || is_white(c)) {
			break;
		}
		next_char(p);
		++i;
	}
	++p->index;
	return chars::view{ p->code.str + s, p->head - s, 0 };
}

static chars::view read_alnums(lexer *p, unsigned &head, unsigned &row, unsigned &col, unsigned &index)
{
	char c;
	unsigned s = p->head;
	unsigned i = 0;
	while (i < sizeof(chars::str) - 1 && p->head < p->code.len) {
		c = p->code.str[p->head];
		if (!is_alnum(c)) {
			break;
		}
		next_char(p);
		++i;
	}
	++p->index;
	return chars::view{ p->code.str + s, p->head - s, 0 };
}

static unsigned chtype(char ch)
{
	return is_alnum(ch) ? 1 : (is_white(ch) ? 0 : 2);
}

static chars::view read(lexer *p, unsigned &head, unsigned &row, unsigned &col, unsigned &index)
{
	skip_white(p);
	head  = p->head;
	row   = p->row;
	col   = p->col;
	index = p->index;
	if (p->head < p->code.len) {
		switch (chtype(p->code.str[p->head])) {
		case 1: return read_alnums(p, head, row, col, index);
		case 2: return read_specials(p, head, row, col, index);
		}
	}
	return chars::view{ p->code.str + p->head, 0, 0 };
}

static token classify(lexer *p, const token *tokens, signed num_tokens, chars::view s, unsigned head, unsigned row, unsigned col, unsigned index)
{
	token t;
	t.user_type = token::STOP_ERR;

	if (s.len > 0 && !is_alnum(s.str[0]) && !is_white(s.str[0])) {
		while (s.len > 0 && t.user_type == token::STOP_ERR) {
			const signed GET_COUNT = 3;
			token (*get[GET_COUNT])(chars::view,const token*,signed) = { get_eof, get_op, get_cmt };
			
			for (signed i = 0; i < GET_COUNT; ++i) {
				t = get[i](s, tokens, num_tokens);
				if (t.user_type != token::STOP_ERR) {
					break;
				}
			}
			if (t.user_type == token::STOP_ERR) {
				--p->col;
				--p->head;
				--p->index;
				--s.len;
			}
		}
	} else {
		const signed GET_COUNT = 5;
		token (*get[GET_COUNT])(chars::view,const token*,signed) = { get_eof, get_lit, get_key, get_alias, get_cmt };

		for (signed i = 0; i < GET_COUNT; ++i) {
			t = get[i](s, tokens, num_tokens);
			if (t.user_type != token::STOP_ERR) {
				break;
			}
		}
	}
	t.head  = head;
	t.row   = row;
	t.col   = col;
	t.index = index;

	if (t.type == token::COMMENT) {
		token eof;
		do {
			s = read(p, head, row, col, index);
			eof = get_eof(s, tokens, num_tokens);
		} while (t.row == row && eof.user_type == token::STOP_ERR);
		eof.head  = head;
		eof.row   = row;
		eof.col   = col;
		eof.index = index;
		return eof.user_type == token::STOP_ERR ? classify(p, tokens, num_tokens, s, head, row, col, index) : eof;
	}

	return t;
}

static token classify(lexer *p, const token *tokens, signed num_tokens)
{
	unsigned head, row, col, index;
	chars::view s = read(p, head, row, col, index);
	return classify(p, tokens, num_tokens, s, head, row, col, index);
}

lexer init_lexer(chars::view code)
{
	return lexer{ code, 0, 0, 0, 0, 0, new_error("<no token>", 10), NULL };
}

token lex(lexer *l, const token *tokens, signed num_tokens)
{
	l->last = classify(l, tokens, num_tokens);
	return l->last;
}

static chars::view readch(lexer *l)
{
	if (l->head >= l->code.len) {
		return chars::view{ l->code.str + l->head, 0, 0 };
	}
	next_char(l);
	++l->index;
	return chars::view{ l->code.str + l->head - 1, 1, 0 };
}

token chlex(lexer *l)
{
	l->last.head        = l->head;
	l->last.row         = l->row;
	l->last.col         = l->col;
	l->last.index       = l->index;
	chars::view s = readch(l);
	l->last.text        = to_chars(s.str, s.len);
	l->last.hashfn      = numhashch;
	l->last.hash        = l->last.hashfn(s.str, s.len);
	l->last.type        = s.len > 0 ? token::CHAR : token::STOP;
	l->last.user_type   = s.len > 0 ? token::CHAR : token::STOP_EOF;
	return l->last;
}

signed count_tokens(token (*lex_fn)(lexer*), lexer l)
{
	signed count = 1;
	token t;
	while ((t = lex_fn(&l)).type != token::STOP) {
		++count;
	}
	return t.user_type == token::STOP_EOF ? count : -1;
}
template < uint64_t byte_count >
class checksum
{
private:
	union {
		uint8_t  u8[byte_count];
		uint32_t u32[byte_count / sizeof(uint32_t)];
	} m_sum;

public:
	checksum( void );
	checksum(const checksum&) = default;
	checksum &operator=(const checksum&) = default;
	operator uint8_t*( void );
	operator const uint8_t*( void ) const;
	uint32_t *u32( void );
	const uint32_t *u32( void ) const;
	char *sprint_hex(char *out) const;
	char *sprint_bin(char *out) const;
	bool operator< (const checksum &r) const;
	bool operator> (const checksum &r) const;
	bool operator<=(const checksum &r) const;
	bool operator>=(const checksum &r) const;
	bool operator==(const checksum &r) const;
	bool operator!=(const checksum &r) const;
};

class crc32
{
private:
	class table
	{
	public:
		uint32_t v[256];
	
	public:
		table( void );
		table(const table&) = default;
		table &operator=(const table&) = default;
	};

public:
	typedef checksum<sizeof(uint32_t)> sum;

private:
	uint32_t     m_sum;
	static table m_table;

public:
	crc32( void );
	crc32(const char *message);
	crc32(const void *message, uint64_t byte_count);
	crc32(const crc32&) = default;
	crc32 &operator=(const crc32&) = default;
	crc32 &operator()(const char *message);
	crc32 &operator()(const void *message, uint64_t byte_count);
	crc32 operator()(const char *message) const;
	crc32 operator()(const void *message, uint64_t byte_count) const;
	void ingest(const char *message);
	void ingest(const void *message, uint64_t byte_count);
	sum digest( void ) const;
	operator sum( void ) const;
};

class fnv1a64
{
public:
	typedef checksum<sizeof(uint64_t)> sum;

private:
	uint64_t m_sum;

public:
	fnv1a64( void );
	fnv1a64(const fnv1a64&) = default;
	fnv1a64 &operator=(const fnv1a64&) = default;
	fnv1a64(const char *message);
	fnv1a64(const void *message, uint64_t byte_count);
	fnv1a64 &operator()(const char *message);
	fnv1a64 &operator()(const void *message, uint64_t byte_count);
	fnv1a64 operator()(const char *message) const;
	fnv1a64 operator()(const void *message, uint64_t byte_count) const;
	void ingest(const char *message);
	void ingest(const void *message, uint64_t byte_count);
	sum digest( void ) const;
	operator sum( void ) const;
};

class md5
{
private:
	static constexpr uint32_t BYTES_PER_DIGEST = 16;
	static constexpr uint32_t WORDS_PER_DIGEST = BYTES_PER_DIGEST / sizeof(uint32_t);
	static constexpr uint32_t BYTES_PER_CHUNK  = 512 / CHAR_BIT;
	static constexpr uint32_t WORDS_PER_CHUNK  = BYTES_PER_CHUNK / sizeof(uint32_t);

public:
	typedef checksum<BYTES_PER_DIGEST> sum;

private:
	union {
		uint32_t u32[WORDS_PER_DIGEST];
		uint8_t  u8[BYTES_PER_DIGEST];
	} m_state;
	union {
		uint32_t u32[WORDS_PER_CHUNK];
		uint8_t  u8[BYTES_PER_CHUNK];
	} m_chunk;
	uint64_t m_message_size;
	uint32_t m_chunk_size;

private:
	static void blit(const uint8_t *src, uint8_t *dst);
	static void blit(const uint8_t *src, uint8_t *dst, uint32_t num);
	static bool is_aligned(const void *mem);
	static uint32_t leftrotate(uint32_t x, uint32_t c);
	void process_chunk(const uint32_t *M, uint32_t *X) const;
	void process_final_chunks(uint32_t *X) const;

public:
	md5( void );
	md5(const char *message);
	md5(const void *message, uint64_t byte_count);
	~md5( void );
	md5(const md5&) = default;
	md5 &operator=(const md5&) = default;
	md5 &operator()(const char *message);
	md5 &operator()(const void *message, uint64_t byte_count);
	md5 operator()(const char *message) const;
	md5 operator()(const void *message, uint64_t byte_count) const;
	void ingest(const char *message);
	void ingest(const void *message, uint64_t byte_count);
	sum digest( void ) const;
	operator sum( void ) const;
};

class sha256
{
private:
	static constexpr uint32_t BITS_PER_BYTE      = CHAR_BIT;
	static constexpr uint32_t BITS_PER_DIGEST    = 256;
	static constexpr uint32_t BITS_PER_BLOCK     = 512;
	static constexpr uint32_t BYTES_PER_DIGEST   = BITS_PER_DIGEST / BITS_PER_BYTE;
	static constexpr uint32_t WORDS_PER_DIGEST   = BYTES_PER_DIGEST / sizeof(uint32_t);
	static constexpr uint32_t BYTES_PER_BLOCK    = BITS_PER_BLOCK / BITS_PER_BYTE;
	static constexpr uint32_t WORDS_PER_BLOCK    = BYTES_PER_BLOCK / sizeof(uint32_t);
	static constexpr uint32_t WORDS_PER_SCHEDULE = 64;
	static constexpr uint8_t  PADDING_CONST      = 1 << 7;

	typedef uint32_t schedule_t[WORDS_PER_SCHEDULE];

public:
	typedef checksum<BYTES_PER_DIGEST> sum;

private:
	union {
		uint32_t u32[WORDS_PER_DIGEST];
		uint8_t  u8[BYTES_PER_DIGEST];
	} m_state;
	union {
		uint32_t u32[WORDS_PER_BLOCK];
		uint8_t  u8[BYTES_PER_BLOCK];
	} m_block;
	uint64_t m_message_size;
	uint32_t m_block_size;

private:
	uint32_t rrot(uint32_t l, uint32_t r) const;
	uint32_t zor(uint32_t a, uint32_t b, uint32_t c) const;
	uint32_t sig(uint32_t x, uint32_t s1, uint32_t s2, uint32_t s3) const;
	uint32_t SIG(uint32_t x, uint32_t s1, uint32_t s2, uint32_t s3) const;
	uint32_t sig0(uint32_t x) const;
	uint32_t sig1(uint32_t x) const;
	uint32_t SIG0(uint32_t x) const;
	uint32_t SIG1(uint32_t x) const;
	uint32_t choice(uint32_t x, uint32_t y, uint32_t z) const;
	uint32_t majority(uint32_t x, uint32_t y, uint32_t z) const;
	void blit(const uint8_t *src, uint8_t *dst) const;
	void blit(const uint8_t *src, uint8_t *dst, uint32_t num) const;
	bool is_aligned(const void *mem) const;
	void create_schedule(const uint8_t *block, schedule_t &schedule) const;
	void process_block(const uint8_t *block, uint32_t *X) const;
	void process_final_blocks(uint32_t *X) const;

public:
	sha256( void );
	sha256(const char *message);
	sha256(const void *message, uint64_t byte_count);
	~sha256( void );
	sha256(const sha256&) = default;
	sha256 &operator=(const sha256&) = default;
	sha256 &operator()(const char *message);
	sha256 &operator()(const void *message, uint64_t byte_count);
	sha256 operator()(const char *message) const;
	sha256 operator()(const void *message, uint64_t byte_count) const;
	void ingest(const char *message);
	void ingest(const void *message, uint64_t byte_count);
	sum digest( void ) const;
	operator sum( void ) const;
};

template < uint64_t byte_count >
checksum<byte_count>::checksum( void )
{
	for (uint64_t i = 0; i < byte_count; ++i) {
		m_sum.u8[i] = 0;
	}
}

template < uint64_t byte_count >
checksum<byte_count>::operator uint8_t*( void )
{
	return m_sum.u8;
}

template < uint64_t byte_count >
checksum<byte_count>::operator const uint8_t*( void ) const
{
	return m_sum.u8;
}

template < uint64_t byte_count >
uint32_t *checksum<byte_count>::u32( void )
{
	return m_sum.u32;
}

template < uint64_t byte_count >
const uint32_t *checksum<byte_count>::u32( void ) const
{
	return m_sum.u32;
}

template < uint64_t byte_count >
char *checksum<byte_count>::sprint_hex(char *out) const
{
	static constexpr char DIGITS[] = "0123456789abcdef";
	for (uint64_t i = 0; i < sizeof(m_sum); ++i, out += 2) {
		uint8_t b = m_sum.u8[i];
		out[0] = DIGITS[b >> 4];
		out[1] = DIGITS[b & 15];
	}
	return out;
}

template < uint64_t byte_count >
char *checksum<byte_count>::sprint_bin(char *out) const
{
	for (uint64_t byte = 0; byte < sizeof(m_sum); ++byte) {
		for (uint64_t bit = 0; bit < CHAR_BIT; ++bit, ++out) {
			out[0] = (m_sum.u8[byte]  & (1 << (CHAR_BIT - 1 - bit))) ? '1' : '0';
		}
	}
	return out;
}

template < uint64_t byte_count >
bool checksum<byte_count>::operator< (const checksum<byte_count> &r) const
{
	for (uint64_t i = 0; i < byte_count; ++i) {
		if (m_sum.u8[i] >= r.m_sum.u8[i]) {
			return false;
		}
	}
	return true;
}

template < uint64_t byte_count >
bool checksum<byte_count>::operator> (const checksum<byte_count> &r) const
{
	for (uint64_t i = 0; i < byte_count; ++i) {
		if (m_sum.u8[i] <= r.m_sum.u8[i]) {
			return false;
		}
	}
	return true;
}

template < uint64_t byte_count >
bool checksum<byte_count>::operator<=(const checksum<byte_count> &r) const
{
	for (uint64_t i = 0; i < byte_count; ++i) {
		if (m_sum.u8[i] > r.m_sum.u8[i]) {
			return false;
		}
	}
	return true;
}

template < uint64_t byte_count >
bool checksum<byte_count>::operator>=(const checksum<byte_count> &r) const
{
	for (uint64_t i = 0; i < byte_count; ++i) {
		if (m_sum.u8[i] < r.m_sum.u8[i]) {
			return false;
		}
	}
	return true;
}

template < uint64_t byte_count >
bool checksum<byte_count>::operator==(const checksum<byte_count> &r) const
{
	for (uint64_t i = 0; i < byte_count; ++i) {
		if (m_sum.u8[i] != r.m_sum.u8[i]) {
			return false;
		}
	}
	return true;
}

template < uint64_t byte_count >
bool checksum<byte_count>::operator!=(const checksum<byte_count> &r) const
{
	for (uint64_t i = 0; i < byte_count; ++i) {
		if (m_sum.u8[i] == r.m_sum.u8[i]) {
			return false;
		}
	}
	return true;
}


typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

static u64 count_ch(const char *s)
{
	u64 c = 0;
	while (s[c++] != 0) {}
	return c - 1;
}

static constexpr u8 ENDIAN_BYTES_32[sizeof(u32)] = { 1, 2, 3, 4 };

static constexpr u8 ENDIAN_BYTES_64[sizeof(u64)] = { 1, 2, 3, 4, 5, 6, 7, 8 };

static bool is_big32( void )
{
	return *reinterpret_cast<const u32*>(ENDIAN_BYTES_32) == 0x01020304;
}

static bool is_lil32( void )
{
	return *reinterpret_cast<const u32*>(ENDIAN_BYTES_32) == 0x4030201;
}

crc32::table crc32::m_table;

crc32::table::table( void )
{
	for (u32 i = 0; i < 256; ++i) {
		v[i] = i;
		for (u32 j = 0; j < 8; ++j) {
			if (v[i] & 1) {
				v[i] = 0xedb88320 ^ (v[i] >> 1);
			} else {
				v[i] >>= 1;
			}
		}
	}
}

crc32::crc32( void ) : m_sum(0)
{}

crc32::crc32(const char *message) : crc32()
{
	ingest(message);
}

crc32::crc32(const void *message, u64 byte_count) : crc32()
{
	ingest(message, byte_count);
}

crc32 &crc32::operator()(const char *message)
{
	ingest(message);
	return *this;
}

crc32 &crc32::operator()(const void *message, u64 byte_count)
{
	ingest(message, byte_count);
	return *this;
}

crc32 crc32::operator()(const char *message) const
{
	return crc32(*this)(message);
}

crc32 crc32::operator()(const void *message, u64 byte_count) const
{
	return crc32(*this)(message, byte_count);
}

void crc32::ingest(const char *message)
{
	ingest(message, count_ch(message));
}

void crc32::ingest(const void *message, u64 byte_count)
{
	m_sum ^= 0xffffffff;
	const u8 *m = (const u8*)message;
	for (u64 i = 0; i < byte_count; ++i) {
		m_sum = m_table.v[(m_sum ^ m[i]) & 0xff] ^ (m_sum >> 8);
	}
	m_sum ^= 0xffffffff;
}

crc32::sum crc32::digest( void ) const
{
	sum s;
	const u8 *r = (const u8*)(&m_sum);
	if (is_lil32()) {
		for (u64 i = 0; i < sizeof(sum); ++i) {
			s[i] = r[sizeof(m_sum) - i - 1];
		}
	} else {
		for (u64 i = 0; i < sizeof(sum); ++i) {
			s[i] = r[i];
		}
	}
	return s;
}

crc32::operator crc32::sum( void ) const
{
	return digest();
}

fnv1a64::fnv1a64( void ) : m_sum(0xcbf29ce484222325ULL)
{}

fnv1a64::fnv1a64(const char *message) : fnv1a64()
{
	ingest(message);
}

fnv1a64::fnv1a64(const void *message, u64 byte_count) : fnv1a64()
{
	ingest(message, byte_count);
}

fnv1a64 &fnv1a64::operator()(const char *message)
{
	ingest(message);
	return *this;
}

fnv1a64 &fnv1a64::operator()(const void *message, u64 byte_count)
{
	ingest(message, byte_count);
	return *this;
}

fnv1a64 fnv1a64::operator()(const char *message) const
{
	return fnv1a64(*this)(message);
}

fnv1a64 fnv1a64::operator()(const void *message, u64 byte_count) const
{
	return fnv1a64(*this)(message, byte_count);
}

void fnv1a64::ingest(const char *message)
{
	ingest(message, count_ch(message));
}

void fnv1a64::ingest(const void *message, u64 byte_count)
{
	const char *m = (const char*)message;
	for (u64 i = 0; i < byte_count; ++i) {
		m_sum ^= u64(m[i]);
		m_sum *= 0x100000001b3ULL;
	}
}

fnv1a64::sum fnv1a64::digest( void ) const
{
	sum s;
	const u8 *r = (const u8*)(&m_sum);
	if (is_lil32()) {
		for (u64 i = 0; i < sizeof(sum); ++i) {
			s[i] = r[sizeof(m_sum) - i - 1];
		}
	} else {
		for (u64 i = 0; i < sizeof(sum); ++i) {
			s[i] = r[i];
		}
	}
	return s;
}

fnv1a64::operator fnv1a64::sum( void ) const
{
	return digest();
}

static constexpr u32 CHUNK_BYTESIZE = 512 / CHAR_BIT;

static constexpr u32 ShiftTable[CHUNK_BYTESIZE] = {
	7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
	5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
	4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
	6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21
};

static constexpr u32 SineTable[CHUNK_BYTESIZE] = {
	0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
	0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
	0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
	0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
	0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
	0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
	0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
	0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
	0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
	0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
	0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
	0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
	0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
	0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
	0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
	0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
};

void md5::blit(const u8 *src, u8 *dst)
{
	memcpy(dst, src, BYTES_PER_CHUNK);
}

void md5::blit(const u8 *src, u8 *dst, u32 num)
{
	memcpy(dst, src, num);
	memset(dst + num, 0, BYTES_PER_CHUNK - num);
}

bool md5::is_aligned(const void *mem)
{
	return (reinterpret_cast<uintptr_t>(mem) & (sizeof(u32) - 1)) != 0;
}

u32 md5::leftrotate(u32 x, u32 c)
{
    return (x << c) | (x >> (32 - c));
}

void md5::process_chunk(const u32 *M, u32 *X) const
{
	enum {a0,b0,c0,d0};

	u32 A = X[a0];
	u32 B = X[b0];
	u32 C = X[c0];
	u32 D = X[d0];
	
	for (u32 i = 0; i < BYTES_PER_CHUNK; ++i) {
		u32 F = 0;
		u32 g = 0;
		if (i < 16) {
			F = (B & C) | ((~B) & D);
			g = i;
		} else if (i < 32) {
			F = (D & B) | ((~D) & C);
			g = (5*i + 1) % 16;
		} else if (i < 48) {
			F = B ^ C ^ D;
			g = (3*i + 5) % 16;
		} else {
			F = C ^ (B | (~D));
			g = (7*i) % 16;
		}
		
		F = F + A + SineTable[i] + M[g];
		A = D;
		D = C;
		C = B;
		B += leftrotate(F, ShiftTable[i]);
	}
	
	X[a0] += A;
	X[b0] += B;
	X[c0] += C;
	X[d0] += D;
}

void md5::process_final_chunks(u32 *X) const
{

	u64 byte_count = m_chunk_size;
	union {
		u32 w32[WORDS_PER_CHUNK];
		u8  w8[BYTES_PER_CHUNK];
	} chunk;

	u32 padding_size = BYTES_PER_CHUNK - (m_message_size % BYTES_PER_CHUNK);
	if (padding_size < sizeof(u64) + sizeof(u8)) {
		padding_size += BYTES_PER_CHUNK;
	}

	memcpy(chunk.w8, m_chunk.u8, byte_count);
	chunk.w8[byte_count] = 0x80;
	memset(chunk.w8 + byte_count + 1, 0, BYTES_PER_CHUNK - (byte_count + 1));
	byte_count += padding_size;

	if (byte_count > BYTES_PER_CHUNK) {
		process_chunk(chunk.w32, X);
		memset(chunk.w8, 0, BYTES_PER_CHUNK - sizeof(u64));
	}

	const u64 ORIGINAL_MESSAGE_BITSIZE = (m_message_size * CHAR_BIT);
	if (is_lil32()) {
		for (u32 i = 0; i < sizeof(u64); ++i) {
			chunk.w8[BYTES_PER_CHUNK - sizeof(u64) + i] = reinterpret_cast<const char*>(&ORIGINAL_MESSAGE_BITSIZE)[i];
		}
	} else {
		for (u32 i = 0; i < sizeof(u64); ++i) {
			chunk.w8[BYTES_PER_CHUNK - 1 - i] = reinterpret_cast<const
			char*>(&ORIGINAL_MESSAGE_BITSIZE)[i];
		}
	}
	process_chunk(chunk.w32, X);
}

md5::md5( void ) : m_message_size(0), m_chunk_size(0)
{
	m_state.u32[0] = 0x67452301;
	m_state.u32[1] = 0xefcdab89;
	m_state.u32[2] = 0x98badcfe;
	m_state.u32[3] = 0x10325476;
}

md5::md5(const char *message) : md5()
{
	ingest(message);
}

md5::md5(const void *message, u64 byte_count) : md5()
{
	ingest(message, byte_count);
}

md5::~md5( void )
{
	memset(m_state.u8, 0, sizeof(m_state.u8));
}

md5 &md5::operator()(const char *message)
{
	ingest(message);
	return *this;
}

md5 &md5::operator()(const void *message, u64 byte_count)
{
	ingest(message, byte_count);
	return *this;
}

md5 md5::operator()(const char *message) const
{
	return md5(*this)(message);
}

md5 md5::operator()(const void *message, u64 byte_count) const
{
	return md5(*this)(message, byte_count);
}

void md5::ingest(const char *message)
{
	ingest(message, u64(strlen(message)));
}

void md5::ingest(const void *message, u64 byte_count)
{
	const u8 *msg = reinterpret_cast<const u8*>(message);
	m_message_size += byte_count;
	while (byte_count > 0) {
		u64 bytes_written = 0;
		if (m_chunk_size == 0 && byte_count >= BYTES_PER_CHUNK && is_aligned(msg)) {
			bytes_written = BYTES_PER_CHUNK;
			process_chunk(reinterpret_cast<const u32*>(msg), m_state.u32);
		} else {
			const u64 BYTES_REMAINING = BYTES_PER_CHUNK - m_chunk_size;
			if (byte_count < BYTES_REMAINING) {
				bytes_written = byte_count;
				m_chunk_size += byte_count;
				blit(msg, m_chunk.u8, bytes_written);
			} else {
				bytes_written = BYTES_REMAINING;
				blit(msg, m_chunk.u8, bytes_written);
				process_chunk(m_chunk.u32, m_state.u32);
				m_chunk_size = 0;
			}
		}

		msg += bytes_written;
		byte_count -= bytes_written;
	}
}

md5::sum md5::digest( void ) const
{
	sum out;
	memcpy(out, m_state.u8, BYTES_PER_DIGEST);
	process_final_chunks(out.u32());
	if (is_big32()) {
		for (u32 i = 0; i < BYTES_PER_DIGEST; i += sizeof(u32)) {
			for (u32 j = 0; j < sizeof(u32) >> 1; ++j) {
				const u32 a = i + j;
				const u32 b = i + sizeof(u32) - j - 1;
				const u8 t = out[a];
				out[a] = out[b];
				out[b] = t;
			}
		}
	}
	return out;
}

md5::operator md5::sum( void ) const
{
	return digest();
}

static constexpr u32 SHA256_INITIAL_HASH_VALUES[8] = {
	0x6a09e667U,
	0xbb67ae85U,
	0x3c6ef372U,
	0xa54ff53aU,
	0x510e527fU,
	0x9b05688cU,
	0x1f83d9abU,
	0x5be0cd19U
};

u32 sha256::rrot(u32 l, u32 r) const
{
	return (l >> r) | (l << (32 - r));
}

u32 sha256::zor(u32 a, u32 b, u32 c) const
{
	return a ^ b ^ c;
}

u32 sha256::sig(u32 x, u32 s1, u32 s2, u32 s3) const
{
	return zor(rrot(x, s1), rrot(x, s2), (x >> s3));
}

u32 sha256::SIG(u32 x, u32 s1, u32 s2, u32 s3) const
{
	return zor(rrot(x, s1), rrot(x, s2), rrot(x, s3));
}

u32 sha256::sig0(u32 x) const
{
	return sig(x, 7, 18, 3);
}

u32 sha256::sig1(u32 x) const
{
	return sig(x, 17, 19, 10);
}

u32 sha256::SIG0(u32 x) const
{
	return SIG(x, 2, 13, 22);
}

u32 sha256::SIG1(u32 x) const
{
	return SIG(x, 6, 11, 25);
}

u32 sha256::choice(u32 x, u32 y, u32 z) const
{
	return (x & y) ^ ((~x) & z);
}

u32 sha256::majority(u32 x, u32 y, u32 z) const
{
	return (x & y) ^ (x & z) ^ (y & z);
}

void sha256::blit(const u8 *src, u8 *dst) const
{
	memcpy(dst, src, BYTES_PER_BLOCK);
}

void sha256::blit(const u8 *src, u8 *dst, u32 num) const
{
	memcpy(dst, src, num);
	memset(dst + num, 0, BYTES_PER_BLOCK - num);
}

bool sha256::is_aligned(const void *mem) const
{
	return (reinterpret_cast<uintptr_t>(mem) & (sizeof(u32) - 1)) != 0;
}

void sha256::create_schedule(const u8 *block, schedule_t &schedule) const
{
	if (is_lil32()) {
		for (u32 i = 0, j = 0; i < 16; i++, j += 4) {
			schedule[i] = (u32(block[j]) << 24) | (u32(block[j + 1]) << 16) | (u32(block[j + 2]) << 8) | u32(block[j + 3]);
		}
	} else {
		memcpy(schedule, block, BYTES_PER_BLOCK);
	}
	for (u32 i = 16; i < WORDS_PER_SCHEDULE; ++i) {
		schedule[i] = sig1(schedule[i-2]) + schedule[i-7] + sig0(schedule[i-15]) + schedule[i-16];
	}
}

void sha256::process_block(const u8 *block, u32 *X) const
{
	static constexpr schedule_t K = {
		0x428a2f98U, 0x71374491U, 0xb5c0fbcfU, 0xe9b5dba5U,
		0x3956c25bU, 0x59f111f1U, 0x923f82a4U, 0xab1c5ed5U,
		0xd807aa98U, 0x12835b01U, 0x243185beU, 0x550c7dc3U,
		0x72be5d74U, 0x80deb1feU, 0x9bdc06a7U, 0xc19bf174U,
		0xe49b69c1U, 0xefbe4786U, 0x0fc19dc6U, 0x240ca1ccU,
		0x2de92c6fU, 0x4a7484aaU, 0x5cb0a9dcU, 0x76f988daU,
		0x983e5152U, 0xa831c66dU, 0xb00327c8U, 0xbf597fc7U,
		0xc6e00bf3U, 0xd5a79147U, 0x06ca6351U, 0x14292967U,
		0x27b70a85U, 0x2e1b2138U, 0x4d2c6dfcU, 0x53380d13U,
		0x650a7354U, 0x766a0abbU, 0x81c2c92eU, 0x92722c85U,
		0xa2bfe8a1U, 0xa81a664bU, 0xc24b8b70U, 0xc76c51a3U,
		0xd192e819U, 0xd6990624U, 0xf40e3585U, 0x106aa070U,
		0x19a4c116U, 0x1e376c08U, 0x2748774cU, 0x34b0bcb5U,
		0x391c0cb3U, 0x4ed8aa4aU, 0x5b9cca4fU, 0x682e6ff3U,
		0x748f82eeU, 0x78a5636fU, 0x84c87814U, 0x8cc70208U,
		0x90befffaU, 0xa4506cebU, 0xbef9a3f7U, 0xc67178f2U
	};

	schedule_t S;
	create_schedule(block, S);

	enum { A, B, C, D, E, F, G, H, REG_COUNT };

	u32 V[REG_COUNT];
	for (u32 i = 0; i < REG_COUNT; ++i) {
		V[i] = X[i];
	}

	for (u32 i = 0; i < WORDS_PER_SCHEDULE; ++i) {
		const u32 T1 = SIG1(V[E]) + choice(V[E], V[F], V[G]) + V[H] + K[i] + S[i];
		const u32 T2 = SIG0(V[A]) + majority(V[A], V[B], V[C]);

		for (u32 j = REG_COUNT - 1; j >= 1; --j) {
			V[j] = V[j-1];
		}
		V[E] += T1;
		V[A] = T1 + T2;
	}

	for (u32 i = 0; i < REG_COUNT; ++i) {
		X[i] += V[i];
	}
}

void sha256::process_final_blocks(u32 *X) const
{
	u64 byte_count = m_block_size;
	union {
		u32 w32[WORDS_PER_BLOCK];
		u8  w8[BYTES_PER_BLOCK];
	} block;

	u32 padding_size = BYTES_PER_BLOCK - (m_message_size % BYTES_PER_BLOCK);
	if (padding_size < sizeof(u64) + sizeof(u8)) {
		padding_size += BYTES_PER_BLOCK;
	}

	memcpy(block.w8, m_block.u8, byte_count);
	block.w8[byte_count] = PADDING_CONST;
	memset(block.w8 + byte_count + 1, 0, BYTES_PER_BLOCK - (byte_count + 1));
	byte_count += padding_size;

	if (byte_count > BYTES_PER_BLOCK) {
		process_block(block.w8, X);
		memset(block.w8, 0, BYTES_PER_BLOCK - sizeof(u64));
	}

	const u64 ORIGINAL_MESSAGE_BITSIZE = (m_message_size * CHAR_BIT);
	if (is_lil32()) {
		for (u32 i = 0; i < sizeof(u64); ++i) {
			block.w8[BYTES_PER_BLOCK - 1 - i] = reinterpret_cast<const
			char*>(&ORIGINAL_MESSAGE_BITSIZE)[i];
		}
	} else {
		for (u32 i = 0; i < sizeof(u64); ++i) {
			block.w8[BYTES_PER_BLOCK - sizeof(u64) + i] = reinterpret_cast<const char*>(&ORIGINAL_MESSAGE_BITSIZE)[i];
		}
	}
	process_block(block.w8, X);
}

sha256::sha256( void ) : m_message_size(0), m_block_size(0)
{
	for (u32 i = 0; i < WORDS_PER_DIGEST; ++i) {
		m_state.u32[i] = SHA256_INITIAL_HASH_VALUES[i];
	}
}

sha256::sha256(const char *message) : sha256()
{
	ingest(message);
}

sha256::sha256(const void *message, u64 byte_count) : sha256()
{
	ingest(message, byte_count);
}

sha256::~sha256( void )
{
	memset(m_block.u8, 0, sizeof(m_block.u8));
}

sha256 &sha256::operator()(const char *message)
{
	ingest(message);
	return *this;
}

sha256 &sha256::operator()(const void *message, u64 byte_count)
{
	ingest(message, byte_count);
	return *this;
}

sha256 sha256::operator()(const char *message) const
{
	return sha256(*this)(message);
}

sha256 sha256::operator()(const void *message, u64 byte_count) const
{
	return sha256(*this)(message, byte_count);
}

void sha256::ingest(const char *message)
{
	ingest(message, strlen(message));
}

void sha256::ingest(const void *message, u64 byte_count)
{	
	const u8 *msg = reinterpret_cast<const u8*>(message);
	m_message_size += byte_count;
	while (byte_count > 0) {
		u64 bytes_written = 0;
		if (m_block_size == 0 && byte_count >= BYTES_PER_BLOCK && is_aligned(msg)) {
			bytes_written = BYTES_PER_BLOCK;
			process_block(msg, m_state.u32);
		} else {
			const u64 BYTES_REMAINING = BYTES_PER_BLOCK - m_block_size;
			if (byte_count < BYTES_REMAINING) {
				bytes_written = byte_count;
				m_block_size += byte_count;
				blit(msg, m_block.u8, bytes_written);
			} else {
				bytes_written = BYTES_REMAINING;
				blit(msg, m_block.u8, bytes_written);
				process_block(m_block.u8, m_state.u32);
				m_block_size = 0;
			}
		}

		msg += bytes_written;
		byte_count -= bytes_written;
	}
}

sha256::sum sha256::digest( void ) const
{
	sum out;
	memcpy(out, m_state.u8, BYTES_PER_DIGEST);
	process_final_blocks(out.u32());
	if (is_lil32()) {
		for (u32 i = 0; i < BYTES_PER_DIGEST; i += sizeof(u32)) {
			for (u32 j = 0; j < sizeof(u32) >> 1; ++j) {
				const u32 a = i + j;
				const u32 b = i + sizeof(u32) - j - 1;
				const u8 t = out[a];
				out[a] = out[b];
				out[b] = t;
			}
		}
	}
	return out;
}

sha256::operator sha256::sum( void ) const
{
	return digest();
}

struct XIS
{
	enum Enum
	{
		NOP    = 0,
		PUT    = 0b10000000 + ( 1<<8),
		PUTS   = 0b10000010 + ( 2<<8),
		PUTI   = 0b10000010 + ( 3<<8),
		CLOCK  = 0b10000000 + ( 4<<8),
		BIN    = 0b00000000 + ( 5<<8),
		AT     = 0b00100000 + ( 6<<8),
		JMP    = 0b01000000 + ( 7<<8),
		SKIP   = 0b01000000 + ( 8<<8),
		ADD    = 0b10010000 + ( 9<<8),
		SUB    = 0b10010000 + (10<<8),
		MUL    = 0b10010000 + (11<<8),
		DIV    = 0b10010000 + (12<<8),
		MOD    = 0b10010000 + (13<<8),
		IADD   = 0b10010100 + (14<<8),
		ISUB   = 0b10010100 + (15<<8),
		IMUL   = 0b10010100 + (16<<8),
		IDIV   = 0b10010100 + (17<<8),
		IMOD   = 0b10010100 + (18<<8),
		INEG   = 0b00010100 + (19<<8),
		LSH    = 0b10010000 + (20<<8),
		RSH    = 0b10010000 + (21<<8),
		AND    = 0b10010000 + (22<<8),
		OR     = 0b10010000 + (23<<8),
		XOR    = 0b10010000 + (24<<8),
		NOT    = 0b00010000 + (25<<8),
		EQ     = 0b10001000 + (26<<8),
		NE     = 0b10001000 + (27<<8),
		LE     = 0b10001000 + (28<<8),
		GE     = 0b10001000 + (29<<8),
		LT     = 0b10001000 + (30<<8),
		GT     = 0b10001000 + (31<<8),
		IEQ    = 0b10001100 + (32<<8),
		INE    = 0b10001100 + (33<<8),
		ILE    = 0b10001100 + (34<<8),
		IGE    = 0b10001100 + (35<<8),
		ILT    = 0b10001100 + (36<<8),
		IGT    = 0b10001100 + (37<<8),
		PORT   = 0b10001001 + (38<<8),
		POLL   = 0b10000001 + (39<<8),
		PASS   = 0b10000001 + (40<<8),
		CPUID  = 0b10000000 + (41<<8),
		PEND   = 0b10000001 + (42<<8),
		PUSH   = 0b10000000 + (43<<8),
		POP    = 0b10000000 + (44<<8),
		TOSS   = 0b10000000 + (45<<8),
		MOVD   = 0b10000000 + (46<<8),
		MOVU   = 0b10000000 + (47<<8),
		PEEK   = 0b00000000 + (48<<8),
		HALT   = 0b00000000 + (49<<8),
		ACK    = 0b10000001 + (50<<8),
		ERR    = 0b10000010 + (51<<8),
		CERR   = 0b10000010 + (52<<8),
		FULL   = 0b10000001 + (53<<8),
		DID    = 0b10000000 + (54<<8),
		
		OFA    = 0b00000010 + (55<<8),
		OFB    = 0b00000010 + (56<<8),
		OFC    = 0b00000010 + (57<<8),

		CJMP   = 0b01001000 + (58<<8),
		CSKIP  = 0b01001000 + (59<<8),
		CNJMP  = 0b01001000 + (60<<8),
		CNSKIP = 0b01001000 + (61<<8),

		DUP    = 0b10000000 + (62<<8),

		SVA    = 0b10000010 + (63<<8),
		SVB    = 0b10000010 + (64<<8),
		SVC    = 0b10000010 + (65<<8),
		LDA    = 0b11000010 + (66<<8),
		LDB    = 0b11000010 + (67<<8),
		LDC    = 0b11000010 + (68<<8),

		RLA    = 0b00100000 + (69<<8),
		RLB    = 0b00100000 + (70<<8),
		RLC    = 0b00100000 + (71<<8),

		TNS    = 0b10000000 + (72<<8),
		TUS    = 0b10000000 + (73<<8),
		TMS    = 0b10000000 + (74<<8),
		TS     = 0b10000000 + (75<<8),
		TM     = 0b10000000 + (76<<8),
		TH     = 0b10000000 + (77<<8),
		TD     = 0b10000000 + (78<<8),
		TW     = 0b10000000 + (79<<8),

		COUNT = 80
	};
};

typedef uint16_t U16;
typedef int16_t  I16;
typedef uint8_t  U8;
typedef int8_t   I8;

static constexpr U16  HILO_IDX = 0x0100;
static const     U8  *HILO_IDX8 = reinterpret_cast<const uint8_t*>(&HILO_IDX);

#define LO8 HILO_IDX8[0]
#define HI8 HILO_IDX8[1]

enum ERRMASK
{
	ERR_DIV0,
	ERR_OVERFLOW,
	ERR_UNDERFLOW,
	ERR_UNDEF,
	ERR_IO
};

union XBYTE
{
	U8 u;
	I8 i;
};

union XWORD
{
	U16   u;
	I16   i;
	XBYTE hilo[2];
};

constexpr U16 UB_MAX = std::numeric_limits<U8 >::max();
constexpr I16 IB_MAX = std::numeric_limits<I8>::max();
constexpr I16 IB_MIN = std::numeric_limits<I8>::min();
constexpr U16 U_MAX  = std::numeric_limits<U16>::max();
constexpr I16 I_MAX  = std::numeric_limits<I16>::max();
constexpr I16 I_MIN  = std::numeric_limits<I16>::min();

constexpr uint32_t MEM_SIZE_MAX = uint32_t(U_MAX) + 1;

struct Addr32
{
	U16 bank;
	U16 loc;

	uint32_t Flat( void ) const { return (uint32_t(bank) << 16) + uint32_t(loc); }
};

static constexpr unsigned XCC_DEFAULT_SYM_CAPACITY  =  128;
static constexpr unsigned XCC_DEFAULT_FILE_CAPACITY = 1024;

struct xcc_text
{
	char          *txt;
	unsigned       len;
	md5::sum  sum;
	xcc_text( void );
	xcc_text(const xcc_text &txt);
	xcc_text &operator=(const xcc_text &txt);
	~xcc_text( void );
};

void xcc_new_text(xcc_text &txt, unsigned len);
void xcc_clear_text(xcc_text &txt);
bool xcc_load_text(const chars::view &filename, xcc_text &out);
bool xcc_strcmp(const char *a, unsigned a_count, const char *b, unsigned b_count);
unsigned xcc_chcount(const char *s);

struct xcc_binary
{
	XWORD    *buffer;
	unsigned  capacity;
	unsigned  size;
};

struct xcc_error
{
	enum {
		NONE,
		MEMORY,
		UNDEF,
		REDEF,
		VERIFY,
		INTERNAL,
		UNEXPECTED,
		MISSING,
		ZERO
	};
	token tok;
	U16         code;
	chars file;
	const char *ifile;
	unsigned    iline;
};

struct xcc_out
{
	lexer l;
	xcc_binary  binary;
	token max;
	U16         errors;
	xcc_error   error;
};

bool xcc_write_word(xcc_binary &buf, XWORD data);

typedef md5 xcc_filehasher;
typedef xcc_filehasher::sum xcc_filesum;

struct xcc_symbol
{
	enum {
		STORAGE_AUTO,
		STORAGE_STATIC,
		STORAGE_PARAM,
		STORAGE_LIT,
		STORAGE_FN,
		STORAGE_LBL
	};
	enum {
		TYPE_SIGNED,
		TYPE_UNSIGNED,
		TYPE_POINTER,
		TYPE_FLOAT,
		TYPE_LBL,
		TYPE_GROUP
	};
	token tok;
	XWORD       data;
	U16         storage;
	U16         type;
	U16         scope_index;
	U16         param_count;
	U16         size;
	U16         link;
	xcc_symbol *param;
	chars file;
};

struct xcc_symbol_stack
{
	xcc_symbol *symbols;
	U16         capacity;
	U16         count;
	U16         scope;
};

U16 xcc_top_scope_stack_size(const xcc_symbol_stack &s);
U16 xcc_loop_stack_size(const xcc_symbol_stack &s, U16 scope_index);

struct xcc_filesums
{
	xcc_filesum *sums;
	unsigned     capacity;
	unsigned     count;
};

bool xcc_add_filesum(xcc_filesums &fs, const xcc_filesum &s);

struct xcc_parser
{
	lexer       in;
	xcc_binary        out;
	token       max;
	xcc_symbol_stack  scopes;
	xcc_symbol       *fn;
	xcc_error         error;
	chars       file;
	xcc_filesums      fsums;
};
xcc_parser xcc_init_parser(lexer l, xcc_binary bin_mem, xcc_symbol *sym_mem, U16 sym_capacity, xcc_filesum *file_mem, unsigned file_capacity);
void xcc_set_error(xcc_parser *p, const token &t, U16 code, const chars &file, const char *ifile, unsigned iline);
bool xcc_push_scope(xcc_parser *p, bool reset_lsp = true);
bool xcc_pop_scope(xcc_parser *p);
bool xcc_write_word(xcc_parser *p, XWORD data);
bool xcc_write_rel(xcc_parser *p, const xcc_symbol *sym, U16 offset = 0);
xcc_symbol *xcc_find_symbol(const chars &name, xcc_parser *p, bool reverse_search = false);
xcc_symbol *xcc_find_var(const chars &name, xcc_parser *p, bool reverse_search = false);
xcc_symbol *xcc_find_lit(const chars &name, xcc_parser *p, bool reverse_search = false);
xcc_symbol *xcc_find_fn(const chars &name, xcc_parser *p, bool reverse_seach = false);
xcc_symbol *xcc_find_lbl(const chars &name, xcc_parser *p, bool reverse_search = false);
xcc_symbol *xcc_add_symbol(const token &tok, unsigned storage, xcc_parser *p, U16 value);
bool xcc_add_memory(xcc_parser *p, U16 size);
xcc_symbol *xcc_add_var(const token &tok, xcc_parser *p);
xcc_symbol *xcc_add_svar(const token &tok, xcc_parser *p);
xcc_symbol *xcc_add_param(const token &tok, xcc_parser *p);
xcc_symbol *xcc_add_lit(const token &tok, U16 value, xcc_parser *p);
xcc_symbol *xcc_add_fn(const token &tok, xcc_parser *p);
xcc_symbol *xcc_add_lbl(const token &tok, xcc_parser *p);
U16 xcc_top_scope_stack_size(const xcc_parser *p);
U16 xcc_loop_stack_size(const xcc_parser *p, U16 loop_scope);
token xcc_peek(xcc_parser *p, token (*lexfn)(lexer*));
bool xcc_match(xcc_parser *p, unsigned type, token *out, token (*lexfn)(lexer*));

struct xcc_path
{
	static constexpr unsigned MAXPATH = 256;

	char     str[MAXPATH];
	unsigned len;

	xcc_path( void );
	xcc_path(const xcc_path&) = default;
	xcc_path &operator=(const xcc_path&) = default;
};

bool xcc_set_path(xcc_path &out, const chars::view &rwd);
chars xcc_short_path(const chars::view &path);

struct xcc_parser_state
{
	xcc_parser        *p;
	xcc_parser         restore_point;
	xcc_path           cwd;
	chars::view  swd;
	unsigned           end;
	unsigned           break_ip;
	unsigned           continue_ip;
	unsigned           loop_scope;
};
xcc_parser_state xcc_new_state(const xcc_parser_state &ps, unsigned end, unsigned break_ip, unsigned continue_ip, unsigned loop_scope);
xcc_parser_state xcc_new_state(xcc_parser *p, const xcc_parser_state *ps, unsigned end, unsigned break_ip, unsigned continue_ip, unsigned loop_scope);
bool xcc_manage_state(xcc_parser_state &ps, bool success);

xcc_text::xcc_text( void ) : txt(NULL), len(0), sum(md5())
{}

xcc_text::xcc_text(const xcc_text &txt) : xcc_text()
{
	xcc_new_text(*this, txt.len);
	for (uint32_t i = 0; i < len; ++i) {
		this->txt[i] = txt.txt[i];
	}
	this->txt[len] = 0;
}

xcc_text &xcc_text::operator=(const xcc_text &txt)
{
	if (this != &txt) {
		xcc_new_text(*this, txt.len);
		for (uint32_t i = 0; i < len; ++i) {
			this->txt[i] = txt.txt[i];
		}
		this->txt[len] = 0;
	}
	return *this;
}

xcc_text::~xcc_text( void )
{
	xcc_clear_text(*this);
}

void xcc_new_text(xcc_text &txt, unsigned len)
{
	xcc_clear_text(txt);
	txt.txt = new char[len + 1];
	txt.len = len;
	txt.txt[len] = 0;
}

void xcc_clear_text(xcc_text &txt)
{
	delete [] txt.txt;
	txt.txt = NULL;
	txt.len = 0;
	txt.sum = md5();
}

bool xcc_load_text(const chars::view &filename, xcc_text &out)
{
	xcc_clear_text(out);
	std::ifstream fin(std::string(filename.str, filename.len));
	if (fin.is_open()) {
		fin.seekg(0, std::ios::end);
		xcc_new_text(out, fin.tellg());
		fin.seekg(0);
		fin.read(out.txt, out.len);
		out.sum = md5(out.txt, out.len);
	} else {
		return false;
	}
	fin.close();
	return true;
}

bool xcc_strcmp(const char *a, unsigned a_count, const char *b, unsigned b_count)
{
	if (a_count != b_count) { return false; }
	for (unsigned i = 0; i < a_count; ++i) {
		if (a[i] != b[i]) {
			return false;
		}
	}
	return true;
}

unsigned xcc_chcount(const char *s)
{
	unsigned n = 0;
	while (s[n] != 0) { ++n; }
	return n;
}

bool xcc_write_word(xcc_binary &buf, XWORD data)
{
	if (buf.size >= buf.capacity) {
		return false;
	}
	buf.buffer[buf.size++] = data;
	return true;
}

static U16 xcc_symbol_stack_size(const xcc_symbol *sym)
{
	return sym->storage != xcc_symbol::STORAGE_STATIC ?
		sym->size :
		(
			sym->size <= 1 ?
				0 :
				1
		);
}

U16 xcc_top_scope_stack_size(const xcc_symbol_stack &s)
{
	U16 size = 0;
	for (signed i = s.count - 1; i >= 0 && s.symbols[i].scope_index == s.scope; --i) {
		size += xcc_symbol_stack_size(s.symbols + i);
	}
	return size;
}

static U16 next_mem_addr(const xcc_symbol_stack &s)
{
	for (signed i = s.count - 1; i >= 0; --i) {
		switch (s.symbols[i].storage) {
		case xcc_symbol::STORAGE_AUTO:
		case xcc_symbol::STORAGE_FN:
		case xcc_symbol::STORAGE_LBL:
		case xcc_symbol::STORAGE_STATIC:
			const U16 next = s.symbols[i].data.u + xcc_symbol_stack_size(s.symbols + i);
			return next > 0 ? next : 1;
		}
	}
	return 1;
}

U16 xcc_loop_stack_size(const xcc_symbol_stack &s, U16 scope_index)
{
	U16 size = 0;
	for (signed i = s.count - 1; i >= 0 && s.symbols[i].scope_index > scope_index; --i) {
		size += xcc_symbol_stack_size(s.symbols + i);
	}
	return size;
}

bool xcc_add_filesum(xcc_filesums &fs, const xcc_filesum &s)
{
	if (fs.count >= fs.capacity) {
		return false;
	}
	for (unsigned i = 0; i < fs.count; ++i) {
		if (fs.sums[i] == s) {
			return false;
		}
	}
	fs.sums[fs.count++] = s;
	return true;
}

chars empty_chars( void )
{
	chars c;
	for (uint32_t i = 0; i < sizeof(c.str); ++i) {
		c.str[i] = 0;
	}
	return c;
}

xcc_parser xcc_init_parser(lexer l, xcc_binary bin_mem, xcc_symbol *sym_mem, U16 sym_capacity, xcc_filesum *file_mem, unsigned file_capacity)
{
	xcc_parser p = {
		l,
		bin_mem,
		l.last,
		xcc_symbol_stack{
			sym_mem,
			sym_capacity,
			0,
			0
		},
		NULL,
		xcc_error{
			new_eof(),
			xcc_error::NONE,
			empty_chars(),
			NULL,
			0
		},
		empty_chars(),
		xcc_filesums{
			file_mem,
			file_capacity,
			0
		}
	};
	return p;
}

void xcc_set_error(xcc_parser *p, const token &t, U16 code, const chars &file, const char *ifile, unsigned iline)
{
	if (p->error.code == xcc_error::NONE) {
		p->error = xcc_error{ t, code, file, ifile, iline };
	}
}

bool xcc_push_scope(xcc_parser *p, bool reset_lsp)
{
	++p->scopes.scope;
	if (reset_lsp) {
		xcc_symbol *sym = xcc_add_symbol(new_alias("", 0, token::ALIAS), xcc_symbol::STORAGE_AUTO, p, 0);
		sym->size = 0;
		return sym != NULL;
	}
	return true;
}

bool xcc_pop_scope(xcc_parser *p)
{
	bool retval = true;
	if (p->scopes.scope >= 1) {
		while (p->scopes.count > 0 && p->scopes.symbols[p->scopes.count - 1].scope_index == p->scopes.scope) {
			--p->scopes.count;
			if (p->scopes.symbols[p->scopes.count].link > 0) {
				xcc_set_error(p, p->scopes.symbols[p->scopes.count].tok, xcc_error::UNDEF, p->file, __FILE__, __LINE__);
				retval = false;
			}
		}
	} else {
		p->scopes.count = 0;
	}
	--p->scopes.scope;
	return retval;
}

bool xcc_write_word(xcc_parser *p, XWORD data)
{
	if (!xcc_write_word(p->out, data)) {
		xcc_set_error(p, p->in.last, xcc_error::MEMORY, p->file, __FILE__, __LINE__);
		return false;
	}
	return true;
}

bool xcc_write_rel(xcc_parser *p, const xcc_symbol *sym, U16 offset)
{
	if (sym->storage == xcc_symbol::STORAGE_LIT) {
		return
			xcc_write_word(p, XWORD{XIS::PUT})                  &&
			xcc_write_word(p, XWORD{U16(sym->data.u + offset)});
	}
	return
		xcc_write_word(p, XWORD{XIS::PUT})                  &&
		xcc_write_word(p, XWORD{U16(sym->data.u + offset)}) &&
		sym->storage == xcc_symbol::STORAGE_STATIC && sym->size <= 1 ? xcc_write_word(p, XWORD{XIS::RLA}) : (
			sym->scope_index > 0 ?
				xcc_write_word(p, XWORD{XIS::RLC}) :
				xcc_write_word(p, XWORD{XIS::RLB})
		);
}

xcc_symbol *xcc_find_symbol(const chars &name, xcc_parser *p, bool reverse_search)
{
	const unsigned name_char_count = xcc_chcount(name.str);
	if (!reverse_search) {
		for (signed i = p->scopes.count - 1; i >= 0; --i) {
			if (xcc_strcmp(name.str, name_char_count, p->scopes.symbols[i].tok.text.str, xcc_chcount(p->scopes.symbols[i].tok.text.str))) {
				return p->scopes.symbols + i;
			}
		}
	} else {
		for (signed i = 0; i < p->scopes.count; ++i) {
			if (xcc_strcmp(name.str, name_char_count, p->scopes.symbols[i].tok.text.str, xcc_chcount(p->scopes.symbols[i].tok.text.str))) {
				return p->scopes.symbols + i;
			}
		}
	}
	return NULL;
}

xcc_symbol *xcc_find_var(const chars &name, xcc_parser *p, bool reverse_search)
{
	xcc_symbol *sym = xcc_find_symbol(name, p, reverse_search);
	if (sym != NULL && sym->storage != xcc_symbol::STORAGE_AUTO && sym->storage != xcc_symbol::STORAGE_STATIC && sym->storage != xcc_symbol::STORAGE_PARAM) {
		return NULL;
	}
	return sym;
}

xcc_symbol *xcc_find_lit(const chars &name, xcc_parser *p, bool reverse_search)
{
	xcc_symbol *sym = xcc_find_symbol(name, p, reverse_search);
	if (sym != NULL && sym->storage != xcc_symbol::STORAGE_LIT) {
		return NULL;
	}
	return sym;
}

xcc_symbol *xcc_find_fn(const chars &name, xcc_parser *p, bool reverse_search)
{
	xcc_symbol *sym = xcc_find_symbol(name, p, reverse_search);
	if (sym != NULL && sym->storage != xcc_symbol::STORAGE_FN) {
		return NULL;
	}
	return sym;
}

xcc_symbol *xcc_find_lbl(const chars &name, xcc_parser *p, bool reverse_search)
{
	xcc_symbol *sym = xcc_find_symbol(name, p, reverse_search);
	if (sym != NULL && sym->storage != xcc_symbol::STORAGE_LBL && sym->scope_index != p->scopes.scope) {
		return NULL;
	}
	return sym;
}

xcc_symbol *xcc_add_symbol(const token &tok, unsigned storage, xcc_parser *p, U16 value)
{
	if (p->scopes.count >= p->scopes.capacity) {
		xcc_set_error(p, tok, xcc_error::MEMORY, p->file, __FILE__, __LINE__);
		return NULL;
	}
	const unsigned name_char_count = xcc_chcount(tok.text.str);
	if (name_char_count > 0) {
		for (signed i = p->scopes.count - 1; i >= 0 && p->scopes.symbols[i].scope_index == p->scopes.scope; --i) {
			if (xcc_strcmp(p->scopes.symbols[i].tok.text.str, xcc_chcount(p->scopes.symbols[i].tok.text.str), tok.text.str, name_char_count)) {
				xcc_set_error(p, tok, xcc_error::REDEF, p->file, __FILE__, __LINE__);
				return NULL;
			}
		}
	}

	xcc_symbol &sym = p->scopes.symbols[p->scopes.count];
	sym.tok         = tok;
	sym.storage     = storage;
	sym.type        = xcc_symbol::TYPE_UNSIGNED;
	sym.scope_index = p->scopes.scope;
	sym.param_count = 0;
	sym.size        = 1;
	sym.param       = NULL;
	sym.data.u      = value;
	sym.link        = 0;
	sym.file        = p->file;
	switch (storage) {
	case xcc_symbol::STORAGE_PARAM:
	case xcc_symbol::STORAGE_LIT:
	case xcc_symbol::STORAGE_STATIC:
		sym.size = 0;
		break;
	case xcc_symbol::STORAGE_AUTO:
	case xcc_symbol::STORAGE_FN:
	case xcc_symbol::STORAGE_LBL:
		sym.size = 1;
		break;
	}
	++p->scopes.count;
	return &sym;
}

bool xcc_add_memory(xcc_parser *p, U16 size)
{
	token empty = new_token("", 0, token::NONE, token::NONE);
	xcc_symbol *sym = xcc_add_symbol(empty, xcc_symbol::STORAGE_AUTO, p, next_mem_addr(p->scopes));
	sym->size = size;
	return sym != NULL;
}

xcc_symbol *xcc_add_var(const token &tok, xcc_parser *p)
{
	return xcc_add_symbol(tok, xcc_symbol::STORAGE_AUTO, p, next_mem_addr(p->scopes));
}

xcc_symbol *xcc_add_svar(const token &tok, xcc_parser *p)
{
	return xcc_add_symbol(tok, xcc_symbol::STORAGE_STATIC, p, p->out.size + 1);
}

xcc_symbol *xcc_add_param(const token &tok, xcc_parser *p)
{
	return xcc_add_symbol(tok, xcc_symbol::STORAGE_PARAM, p, 0);
}

xcc_symbol *xcc_add_lit(const token &tok, U16 value, xcc_parser *p)
{
	return xcc_add_symbol(tok, xcc_symbol::STORAGE_LIT, p, value);
}

xcc_symbol *xcc_add_fn(const token &tok, xcc_parser *p)
{
	xcc_symbol *sym = xcc_add_symbol(tok, xcc_symbol::STORAGE_FN, p, next_mem_addr(p->scopes));
	if (sym != NULL) {
		sym->link = p->out.size;
	}
	return sym;
}

xcc_symbol *xcc_add_lbl(const token &tok, xcc_parser *p)
{
	xcc_symbol *sym = xcc_add_symbol(tok, xcc_symbol::STORAGE_LBL, p, next_mem_addr(p->scopes));
	if (sym != NULL) {
		sym->link = p->out.size;
	}
	return sym;
}

U16 xcc_top_scope_stack_size(const xcc_parser *p)
{
	return xcc_top_scope_stack_size(p->scopes);
}

U16 xcc_loop_stack_size(const xcc_parser *p, U16 loop_scope)
{
	return xcc_loop_stack_size(p->scopes, loop_scope);
}

token xcc_peek(xcc_parser *p, token (*lexfn)(lexer*))
{
	lexer l = p->in;
	return lexfn(&l);
}

bool xcc_match(xcc_parser *p, unsigned type, token *out, token (*lexfn)(lexer*))
{
	lexer l = p->in;
	lexfn(&l);

	if (out != NULL) {
		*out = l.last;
	}

	if (l.last.index >= p->max.index) {
		p->max = l.last;
	}

	if (l.last.type == token::STOP) {
		return type == token::STOP_EOF;
	}

	if (type == l.last.user_type) {
		p->in = l;
		++p->in.index;
		return true;
	}

	return false;
}

xcc_path::xcc_path( void ) : len(0)
{
	for (uint32_t i = 0; i < len; ++i) {
		str[i] = 0;
	}
}

bool xcc_set_path(xcc_path &out, const chars::view &rwd)
{
	static const token tokens[] = {
		new_operator("..", 2, 1)
	};

	lexer l = init_lexer(rwd);

	while (out.len > 0 && out.str[out.len - 1] != '/' && out.str[out.len - 1] != '\\') {
		--out.len;
	}

	if (rwd.len > 0 && rwd.str != NULL) {
		lexer t = l;
		while (lex(&t, tokens, 1).user_type != token::STOP_EOF) {
			if (t.last.user_type == 1) {
				while (out.len > 0 && out.str[out.len - 1] != '/' && out.str[out.len - 1] != '\\') {
					--out.len;
				}
			} else {
				t = l;
				chlex(&t);
				const uint32_t len = xcc_chcount(t.last.text.str);
				for (uint32_t i = 0; i < len; ++i) {
					out.str[out.len] = t.last.text.str[i];
					++out.len;
					if (out.len == xcc_path::MAXPATH) {
						out.str[out.len - 1] = '\0';
						return false;
					}
				}
			}
			l = t;
		}
	}
	for (uint32_t i = out.len; i < xcc_path::MAXPATH; ++i) {
		out.str[i] = '\0';
	}
	return true;
}

chars xcc_short_path(const chars::view &path)
{
	static constexpr uint32_t MAXLEN = sizeof(chars::str) - 1;
	chars c;
	if (path.len <= MAXLEN) {
		for (uint32_t i = 0; i < path.len; ++i) {
			c.str[i] = path.str[i];
		}
		for (uint32_t i = path.len; i <= MAXLEN; ++i) {
			c.str[i] = 0;
		}
	} else {
		c.str[0] = c.str[1] = c.str[2] = '.';
		for (uint32_t i = 0; i < MAXLEN - 3; ++i) {
			c.str[MAXLEN - i - 1] = path.str[path.len - i - 1];
		}
		c.str[MAXLEN] = 0;
	}
	return c;
}

xcc_parser_state xcc_new_state(const xcc_parser_state &ps, unsigned end, unsigned break_ip, unsigned continue_ip, unsigned loop_scope)
{
	xcc_parser_state nps = {
		ps.p,
		*ps.p,
		ps.cwd,
		ps.swd,
		end,
		break_ip,
		continue_ip,
		loop_scope
	};
	nps.p->file = xcc_short_path(chars::view{ nps.cwd.str, nps.cwd.len, 0 });
	return nps;
}

xcc_parser_state xcc_new_state(xcc_parser *p, const xcc_parser_state *ps, unsigned end, unsigned break_ip, unsigned continue_ip, unsigned loop_scope)
{
	xcc_parser_state nps = {
		p,
		*p,
		ps != NULL ? ps->cwd : xcc_path(),
		ps != NULL ? ps->swd : chars::view{ "", 0, 0 },
		end,
		break_ip,
		continue_ip,
		loop_scope
	};
	nps.p->file = xcc_short_path(chars::view{ nps.cwd.str, nps.cwd.len, 0 });
	return nps;
}

bool xcc_manage_state(xcc_parser_state &ps, bool success)
{
	if (!success) {
		ps.restore_point.error = ps.p->error;
		ps.restore_point.max = ps.p->max.index >= ps.restore_point.max.index ? ps.p->max : ps.restore_point.max;
		*ps.p = ps.restore_point;
	} else {
		ps.p->file = ps.restore_point.file;
	}
	return success;
}

xcc_out xasm(lexer l, xcc_binary memory, const U16 sym_capacity = XCC_DEFAULT_SYM_CAPACITY, const unsigned file_capacity = XCC_DEFAULT_FILE_CAPACITY);
bool xasm_inline(xcc_parser_state ps);

#define new_state(end) xcc_new_state(ps, end, ps.break_ip, ps.continue_ip, ps.loop_scope)
#define manage_state(success) xcc_manage_state(ps, ps.p->error.code == xcc_error::NONE && (success))
#define set_error(p, t, code) xcc_set_error(p, t, code, p->file, __FILE__, __LINE__)

static unsigned hex2u(const char *nums, unsigned len)
{
	unsigned h = 0;
	for (unsigned i = 2; i < len; ++i) {
		if (nums[i] >= '0' && nums[i] <= '9') {
			h = h  * 16 + nums[i] - '0';
		} else if (nums[i] >= 'a' && nums[i] <= 'f') {
			h = h  * 16 + nums[i] - 'a' + 10;
		} else if (nums[i] >= 'A' && nums[i] <= 'F') {
			h = h  * 16 + nums[i] - 'A' + 10;
		}
	}
	return h;
}

struct xtoken
{
	enum tokentype
	{
		KEYWORD_INSTRUCTION_SET = 1,

		KEYWORD_DIRECTIVE_BIN,
		KEYWORD_DIRECTIVE_SCOPE,
		
		KEYWORD_DIRECTIVE_LIT,
		KEYWORD_DIRECTIVE_HERE,
		KEYWORD_DIRECTIVE_TOP,
		KEYWORD_DIRECTIVE_FRAME,
		KEYWORD_DIRECTIVE_BASE,
		KEYWORD_DIRECTIVE_ENTRY,
		KEYWORD_DIRECTIVE_ERR,

		OPERATOR_DIRECTIVE_AT,
		OPERATOR_DIRECTIVE_ADDR,
		OPERATOR_DIRECTIVE_DOLLAR,
	
		OPERATOR_ENCLOSE_PARENTHESIS_L,
		OPERATOR_ENCLOSE_PARENTHESIS_R,
	
		OPERATOR_ENCLOSE_BRACKET_L,
		OPERATOR_ENCLOSE_BRACKET_R,
	
		OPERATOR_ENCLOSE_BRACE_L,
		OPERATOR_ENCLOSE_BRACE_R,
		
		OPERATOR_ARITHMETIC_ADD,
		OPERATOR_ARITHMETIC_SUB,
		OPERATOR_ARITHMETIC_MUL,
		OPERATOR_ARITHMETIC_DIV,
		OPERATOR_ARITHMETIC_MOD,
		OPERATOR_BITWISE_AND,
		OPERATOR_LOGICAL_AND,
		OPERATOR_BITWISE_OR,
		OPERATOR_LOGICAL_OR,
		OPERATOR_BITWISE_XOR,
		OPERATOR_BITWISE_NOT,
		OPERATOR_BITWISE_LSHIFT,
		OPERATOR_BITWISE_RSHIFT,
		OPERATOR_LOGICAL_LESS,
		OPERATOR_LOGICAL_LESSEQUAL,
		OPERATOR_LOGICAL_GREATER,
		OPERATOR_LOGICAL_GREATEREQUAL,
		OPERATOR_LOGICAL_EQUAL,
		OPERATOR_LOGICAL_NOTEQUAL,
		OPERATOR_LOGICAL_NOT,

		OPERATOR_STOP,
		OPERATOR_COMMA,
		OPERATOR_COLON,

		OPERATOR_REVERSE_SEARCH,

		LITERAL_INT
	};
};

const signed X_TOKEN_COUNT = 103;
const token X_TOKENS[X_TOKEN_COUNT] = {
	new_keyword ("nop",                     3, XIS::NOP),
	new_keyword ("at",                      2, XIS::AT),
	new_keyword ("set",                     3, xtoken::KEYWORD_INSTRUCTION_SET),
	new_keyword ("put",                     3, XIS::PUT),
	new_keyword ("add",                     3, XIS::ADD),
	new_keyword ("sub",                     3, XIS::SUB),
	new_keyword ("mul",                     3, XIS::MUL),
	new_keyword ("div",                     3, XIS::DIV),
	new_keyword ("mod",                     3, XIS::MOD),
	new_keyword ("iadd",                    4, XIS::IADD),
	new_keyword ("isub",                    4, XIS::ISUB),
	new_keyword ("imul",                    4, XIS::IMUL),
	new_keyword ("idiv",                    4, XIS::IDIV),
	new_keyword ("imod",                    4, XIS::IMOD),
	new_keyword ("lsh",                     3, XIS::LSH),
	new_keyword ("rsh",                     3, XIS::RSH),
	new_keyword ("and",                     3, XIS::AND),
	new_keyword ("or",                      2, XIS::OR),
	new_keyword ("xor",                     3, XIS::XOR),
	new_keyword ("mov",                     3, XIS::MOVU),
	new_keyword ("eq",                      2, XIS::EQ),
	new_keyword ("ne",                      2, XIS::NE),
	new_keyword ("gt",                      2, XIS::GT),
	new_keyword ("lt",                      2, XIS::LT),
	new_keyword ("ge",                      2, XIS::GE),
	new_keyword ("le",                      2, XIS::LE),
	new_keyword ("igt",                     3, XIS::IGT),
	new_keyword ("ilt",                     3, XIS::ILT),
	new_keyword ("ige",                     3, XIS::IGE),
	new_keyword ("ile",                     3, XIS::ILE),
	new_keyword ("jmp",                     3, XIS::JMP),
	new_keyword ("cjmp",                    4, XIS::CJMP),
	new_keyword ("skip",                    4, XIS::SKIP),
	new_keyword ("cskip",                   5, XIS::CSKIP),
	new_keyword ("clock",                   5, XIS::CLOCK),
	new_keyword("sva",                      3, XIS::SVA),
	new_keyword("lda",                      3, XIS::LDA),
	new_keyword("rla",                      3, XIS::RLA),
	new_keyword("svb",                      3, XIS::SVB),
	new_keyword("ldb",                      3, XIS::LDB),
	new_keyword("rlb",                      3, XIS::RLB),
	new_keyword("svc",                      3, XIS::SVC),
	new_keyword("ldc",                      3, XIS::LDC),
	new_keyword("rlc",                      3, XIS::RLC),
	new_keyword("port",                     4, XIS::PORT),
	new_keyword("poll",                     4, XIS::POLL),
	new_keyword("pass",                     4, XIS::PASS),
	new_keyword("pend",                     4, XIS::PEND),
	new_keyword("ack",                      3, XIS::ACK),
	new_keyword("cpuid",                    5, XIS::CPUID),
	new_keyword("did",                      3, XIS::DID),
	new_keyword("cerr",                     4, XIS::CERR),
	new_keyword("full",                     4, XIS::FULL),
	new_keyword("dup",                      3, XIS::DUP),
	new_keyword("ofa",                      3, XIS::OFA),
	new_keyword("ofb",                      3, XIS::OFB),
	new_keyword("ofc",                      3, XIS::OFC),

	new_operator("@",                       1, xtoken::OPERATOR_DIRECTIVE_AT),
	new_operator("&",                       1, xtoken::OPERATOR_DIRECTIVE_ADDR),
	new_operator("$",                       1, xtoken::OPERATOR_DIRECTIVE_DOLLAR),
	
	new_keyword ("bin",                     3, xtoken::KEYWORD_DIRECTIVE_BIN),
	new_keyword ("scope",                   5, xtoken::KEYWORD_DIRECTIVE_SCOPE),
	new_keyword ("here",                    4, xtoken::KEYWORD_DIRECTIVE_HERE),
	new_keyword ("top",                     3, xtoken::KEYWORD_DIRECTIVE_TOP),
	new_keyword ("frame",                   5, xtoken::KEYWORD_DIRECTIVE_FRAME),
	new_keyword ("base",                    4, xtoken::KEYWORD_DIRECTIVE_BASE),
	new_keyword ("entry",                   5, xtoken::KEYWORD_DIRECTIVE_ENTRY),
	new_keyword ("lit",                     3, xtoken::KEYWORD_DIRECTIVE_LIT),
	new_keyword ("err",                     3, xtoken::KEYWORD_DIRECTIVE_ERR),

	new_operator("+",                       1, xtoken::OPERATOR_ARITHMETIC_ADD),
	new_operator("-",                       1, xtoken::OPERATOR_ARITHMETIC_SUB),
	new_operator("*",                       1, xtoken::OPERATOR_ARITHMETIC_MUL),
	new_operator("/",                       1, xtoken::OPERATOR_ARITHMETIC_DIV),
	new_operator("%",                       1, xtoken::OPERATOR_ARITHMETIC_MOD),
	new_operator("&",                       1, xtoken::OPERATOR_BITWISE_AND),
	new_operator("&&",                      2, xtoken::OPERATOR_LOGICAL_AND),
	new_operator("|",                       1, xtoken::OPERATOR_BITWISE_OR),
	new_operator("||",                      2, xtoken::OPERATOR_LOGICAL_OR),
	new_operator("^",                       1, xtoken::OPERATOR_BITWISE_XOR),
	new_operator("~",                       1, xtoken::OPERATOR_BITWISE_NOT),
	new_operator("<<",                      2, xtoken::OPERATOR_BITWISE_LSHIFT),
	new_operator(">>",                      2, xtoken::OPERATOR_BITWISE_RSHIFT),
	new_operator("<",                       1, xtoken::OPERATOR_LOGICAL_LESS),
	new_operator("<=",                      2, xtoken::OPERATOR_LOGICAL_LESSEQUAL),
	new_operator(">",                       1, xtoken::OPERATOR_LOGICAL_GREATER),
	new_operator(">=",                      2, xtoken::OPERATOR_LOGICAL_GREATEREQUAL),
	new_operator("==",                      2, xtoken::OPERATOR_LOGICAL_EQUAL),
	new_operator("!=",                      2, xtoken::OPERATOR_LOGICAL_NOTEQUAL),
	new_operator("!",                       1, xtoken::OPERATOR_LOGICAL_NOT),

	new_operator(":",                       1, xtoken::OPERATOR_COLON),
	new_operator("[",                       1, xtoken::OPERATOR_ENCLOSE_BRACKET_L),
	new_operator("]",                       1, xtoken::OPERATOR_ENCLOSE_BRACKET_R),
	new_operator("{",                       1, xtoken::OPERATOR_ENCLOSE_BRACE_L),
	new_operator("}",                       1, xtoken::OPERATOR_ENCLOSE_BRACE_R),
	new_operator("(",                       1, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_L),
	new_operator(")",                       1, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_R),
	new_operator(",",                       1, xtoken::OPERATOR_COMMA),
	new_operator(".",                       1, xtoken::OPERATOR_STOP),
	new_comment ("//",                      2),
	new_operator("::",                      2, xtoken::OPERATOR_REVERSE_SEARCH),
	new_alias   ("[a-zA-Z_][a-zA-Z0-9_]*", 22,  token::ALIAS),
	new_literal ("[0-9]+",                  6, xtoken::LITERAL_INT),
	new_literal ("0[xX][0-9a-fA-F]+",      17, xtoken::LITERAL_INT, hex2u)
};

static token xasm_lex(lexer *l)
{
	return lex(l, X_TOKENS, X_TOKEN_COUNT);
}

static token xasm_peek(xcc_parser *p)
{
	return xcc_peek(p, xasm_lex);
}

static bool xasm_match(xcc_parser *p, unsigned type, token *out = NULL)
{
	return xcc_match(p, type, out, xasm_lex);
}

static bool xasm_try_decl_var        (xcc_parser_state ps);
static bool xasm_try_decl_mem        (xcc_parser_state ps);
static bool xasm_try_decl_list       (xcc_parser_state ps);
static bool xasm_try_directive_scope (xcc_parser_state ps);
static bool xasm_try_directive_stmt  (xcc_parser_state ps);
static bool xasm_try_param           (xcc_parser_state ps);
static bool xasm_try_lparam          (xcc_parser_state ps);
static bool xasm_try_put_param       (xcc_parser_state ps);
static bool xasm_try_directive_at    (xcc_parser_state ps);
static bool xasm_try_instruction_put (xcc_parser_state ps);
static bool xasm_try_mov_param       (xcc_parser_state ps);
static bool xasm_try_repeat_mov_param(xcc_parser_state ps);
static bool xasm_try_instruction_mov (xcc_parser_state ps);
static bool xasm_try_instruction_stmt(xcc_parser_state ps);
static bool xasm_try_emit_lit_list   (xcc_parser_state ps);
static bool xasm_try_statements      (xcc_parser_state ps);
static bool xasm_try_program         (xcc_parser_state ps);

template < typename type_t >
static bool xasm_eval_operation(xcc_parser *p, unsigned user_type, type_t &l, type_t r)
{
	switch (user_type) {
	case xtoken::OPERATOR_ARITHMETIC_ADD:       l += r;       return true;
	case xtoken::OPERATOR_ARITHMETIC_SUB:       l -= r;       return true;
	case xtoken::OPERATOR_ARITHMETIC_MUL:       l *= r;       return true;
	case xtoken::OPERATOR_ARITHMETIC_DIV:       l /= r;       return true;
	case xtoken::OPERATOR_ARITHMETIC_MOD:       l %= r;       return true;
	case xtoken::OPERATOR_BITWISE_AND:
	case xtoken::OPERATOR_LOGICAL_AND:          l &= r;       return true;
	case xtoken::OPERATOR_BITWISE_OR:
	case xtoken::OPERATOR_LOGICAL_OR:           l |= r;       return true;
	case xtoken::OPERATOR_BITWISE_XOR:          l ^= r;       return true;
	case xtoken::OPERATOR_BITWISE_NOT:          l = ~r;       return true;
	case xtoken::OPERATOR_BITWISE_LSHIFT:       l <<= r;      return true;
	case xtoken::OPERATOR_BITWISE_RSHIFT:       l >>= r;      return true;
	case xtoken::OPERATOR_LOGICAL_LESS:         l = (l < r);  return true;
	case xtoken::OPERATOR_LOGICAL_LESSEQUAL:    l = (l <= r); return true;
	case xtoken::OPERATOR_LOGICAL_GREATER:      l = (l > r);  return true;
	case xtoken::OPERATOR_LOGICAL_GREATEREQUAL: l = (l >= r); return true;
	case xtoken::OPERATOR_LOGICAL_EQUAL:        l = (l == r); return true;
	case xtoken::OPERATOR_LOGICAL_NOTEQUAL:     l = (l != r); return true;
	case xtoken::OPERATOR_LOGICAL_NOT:          l = !r;       return true;
	}
	return false;
}

static bool xasm_try_alias(xcc_parser_state ps, token &t, bool &reverse_search)
{
	reverse_search = false;
	if (
		manage_state(
			(
				reverse_search = xasm_match(ps.p, xtoken::OPERATOR_REVERSE_SEARCH) &&
				xasm_match(ps.p, token::ALIAS, &t)
			) ||
			xasm_match(ps.p, token::ALIAS, &t)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xasm_try_lit_factor(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xasm_try_read_lit(xcc_parser_state ps, type_t &result)
{
	token t;
	bool rs;
	if (xasm_match(ps.p, xtoken::LITERAL_INT, &t)) {
		result = t.hash;
		return true;
	} else if (xasm_try_alias(new_state(ps.end), t, rs)) {
		const xcc_symbol *sym = xcc_find_lit(t.text, ps.p, rs);
		if (sym != NULL) {
			result = sym->data.u;
			return true;
		}
	}
	return false;
}

template < typename type_t >
static bool xasm_try_lit_opt_factor(xcc_parser_state ps, type_t &l)
{
	token t;
	while (xasm_match(ps.p, xtoken::OPERATOR_ARITHMETIC_MUL, &t) || xasm_match(ps.p, xtoken::OPERATOR_ARITHMETIC_DIV, &t) || xasm_match(ps.p, xtoken::OPERATOR_ARITHMETIC_MOD, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				xasm_try_lit_factor(new_state(ps.end), r)    &&
				xasm_eval_operation(ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xasm_try_lit_rval(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xasm_try_lit_bwnot_val(xcc_parser_state ps, type_t &l)
{
	token t;
	if (
		manage_state(
			xasm_match         (ps.p, xtoken::OPERATOR_BITWISE_NOT, &t) &&
			xasm_try_lit_rval  (new_state(ps.end), l)                   &&
			xasm_eval_operation(ps.p, t.user_type, l, l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xasm_try_lit_lnot_val(xcc_parser_state ps, type_t &l)
{
	token t;
	if (
		manage_state(
			xasm_match         (ps.p, xtoken::OPERATOR_LOGICAL_NOT, &t) &&
			xasm_try_lit_rval  (new_state(ps.end), l)                   &&
			xasm_eval_operation(ps.p, t.user_type, l, l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xasm_try_lit_rval(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xasm_try_lit_bwnot_val(new_state(ps.end), l) ||
			xasm_try_lit_lnot_val (new_state(ps.end), l) ||
			xasm_try_read_lit     (new_state(ps.end), l) ||
			(
				(
					xasm_match       (ps.p, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)         &&
					xasm_try_lit_expr(new_state(xtoken::OPERATOR_ENCLOSE_PARENTHESIS_R), l) &&
					xasm_match       (ps.p, xtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
				)
			)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xasm_try_lit_uni_pos(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xasm_match       (ps.p, xtoken::OPERATOR_ARITHMETIC_ADD) &&
			xasm_try_lit_rval(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xasm_try_lit_uni_neg(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xasm_match       (ps.p, xtoken::OPERATOR_ARITHMETIC_SUB) &&
			xasm_try_lit_rval(new_state(ps.end), l)
		)
	) {
		l = -l;
		return true;
	}
	return false;
}

template < typename type_t >
static bool xasm_try_lit_factor(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xasm_try_lit_uni_pos (new_state(ps.end), l) ||
			xasm_try_lit_uni_neg (new_state(ps.end), l) ||
			xasm_try_lit_rval    (new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xasm_try_lit_term(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xasm_try_lit_opt_term(xcc_parser_state ps, type_t &l)
{
	token t;
	while (xasm_match(ps.p, xtoken::OPERATOR_ARITHMETIC_ADD, &t) || xasm_match(ps.p, xtoken::OPERATOR_ARITHMETIC_SUB, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				xasm_try_lit_term  (new_state(ps.end), r)    &&
				xasm_eval_operation(ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xasm_try_lit_term(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xasm_try_lit_factor    (new_state(ps.end), l) &&
			xasm_try_lit_opt_factor(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xasm_try_lit_bitshift(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xasm_try_lit_opt_bitshift(xcc_parser_state ps, type_t &l)
{
	token t;
	while (xasm_match(ps.p, xtoken::OPERATOR_BITWISE_LSHIFT, &t) || xasm_match(ps.p, xtoken::OPERATOR_BITWISE_RSHIFT, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				xasm_try_lit_bitshift(new_state(ps.end), r)    &&
				xasm_eval_operation  (ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xasm_try_lit_bitshift(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xasm_try_lit_term    (new_state(ps.end), l) &&
			xasm_try_lit_opt_term(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xasm_try_lit_less_greater(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xasm_try_lit_opt_less_greater(xcc_parser_state ps, type_t &l)
{
	token t;
	while (xasm_match(ps.p, xtoken::OPERATOR_LOGICAL_LESS, &t) || xasm_match(ps.p, xtoken::OPERATOR_LOGICAL_LESSEQUAL, &t) || xasm_match(ps.p, xtoken::OPERATOR_LOGICAL_GREATER, &t) || xasm_match(ps.p, xtoken::OPERATOR_LOGICAL_GREATEREQUAL, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				xasm_try_lit_less_greater(new_state(ps.end), r)    &&
				xasm_eval_operation      (ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xasm_try_lit_less_greater(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xasm_try_lit_bitshift    (new_state(ps.end), l) &&
			xasm_try_lit_opt_bitshift(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xasm_try_lit_equality(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xasm_try_lit_opt_equality(xcc_parser_state ps, type_t &l)
{
	token t;
	while (xasm_match(ps.p, xtoken::OPERATOR_LOGICAL_EQUAL, &t) || xasm_match(ps.p, xtoken::OPERATOR_LOGICAL_NOTEQUAL, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				xasm_try_lit_equality(new_state(ps.end), r)    &&
				xasm_eval_operation  (ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xasm_try_lit_equality(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xasm_try_lit_less_greater    (new_state(ps.end), l) &&
			xasm_try_lit_opt_less_greater(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xasm_try_lit_and(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xasm_try_lit_opt_and(xcc_parser_state ps, type_t &l)
{
	token t;
	while (xasm_match(ps.p, xtoken::OPERATOR_BITWISE_AND, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				xasm_try_lit_and   (new_state(ps.end), r)    &&
				xasm_eval_operation(ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xasm_try_lit_and(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xasm_try_lit_equality    (new_state(ps.end), l) &&
			xasm_try_lit_opt_equality(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xasm_try_lit_xor(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xasm_try_lit_opt_xor(xcc_parser_state ps, type_t &l)
{
	token t;
	while (xasm_match(ps.p, xtoken::OPERATOR_BITWISE_XOR, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				xasm_try_lit_xor   (new_state(ps.end), r)    &&
				xasm_eval_operation(ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xasm_try_lit_xor(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xasm_try_lit_and    (new_state(ps.end), l) &&
			xasm_try_lit_opt_and(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xasm_try_lit_or(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xasm_try_lit_opt_or(xcc_parser_state ps, type_t &l)
{
	token t;
	while (xasm_match(ps.p, xtoken::OPERATOR_BITWISE_OR, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				xasm_try_lit_or    (new_state(ps.end), r)    &&
				xasm_eval_operation(ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xasm_try_lit_or(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xasm_try_lit_xor    (new_state(ps.end), l) &&
			xasm_try_lit_opt_xor(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xasm_try_lit_logical_and(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xasm_try_lit_opt_logical_and(xcc_parser_state ps, type_t &l)
{
	token t;
	while (xasm_match(ps.p, xtoken::OPERATOR_LOGICAL_AND, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				xasm_try_lit_logical_and(new_state(ps.end), r)    &&
				xasm_eval_operation     (ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xasm_try_lit_logical_and(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xasm_try_lit_or    (new_state(ps.end), l) &&
			xasm_try_lit_opt_or(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xasm_try_lit_logical_or(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xasm_try_lit_opt_logical_or(xcc_parser_state ps, type_t &l)
{
	token t;
	while (xasm_match(ps.p, xtoken::OPERATOR_LOGICAL_OR, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				xasm_try_lit_logical_or(new_state(ps.end), r)    &&
				xasm_eval_operation    (ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xasm_try_lit_logical_or(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xasm_try_lit_logical_and    (new_state(ps.end), l) &&
			xasm_try_lit_opt_logical_and(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xasm_try_lit_expr(xcc_parser_state ps, type_t &l)
{

	if (
		manage_state(
			xasm_try_lit_logical_or    (new_state(ps.end), l) &&
			xasm_try_lit_opt_logical_or(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_decl_var(xcc_parser_state ps)
{
	const token t = xasm_peek(ps.p);
	if (
		manage_state(
			xasm_match(ps.p, token::ALIAS)
		)
	) {
		if (xcc_add_var(t, ps.p) == NULL) { return false; }
		return true;
	}
	return false;
}

static bool xasm_try_decl_mem(xcc_parser_state ps)
{
	U16 size;
	if (
		manage_state(
			xasm_match         (ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_L)            &&
			xasm_try_read_lit  (new_state(xtoken::OPERATOR_ENCLOSE_BRACKET_R), size) &&
			xasm_match         (ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_R)            &&
			xcc_add_memory(ps.p, size)
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_decl_list(xcc_parser_state ps)
{
	if (
		manage_state(
			xasm_peek(ps.p).user_type == ps.end ||
			(
				(
					xasm_try_decl_var(new_state(ps.end)) ||
					xasm_try_decl_mem(new_state(ps.end))
				) &&
				(
					!xasm_match       (ps.p, xtoken::OPERATOR_COMMA) ||
					xasm_try_decl_list(new_state(ps.end))
				)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_directive_scope(xcc_parser_state ps)
{
	U16 stack_size;
	if (
		manage_state(
			xasm_match         (ps.p, xtoken::KEYWORD_DIRECTIVE_SCOPE)       &&
			xasm_match         (ps.p, xtoken::OPERATOR_COLON)                &&
			xcc_push_scope(ps.p, false)                                      &&
			xasm_try_decl_list (new_state(xtoken::OPERATOR_ENCLOSE_BRACE_L)) &&
			(
				(stack_size = xcc_top_scope_stack_size(ps.p)) == 0 ||
				(
					xcc_write_word(ps.p, XWORD{XIS::PUT})   &&
					xcc_write_word(ps.p, XWORD{stack_size}) &&
					xcc_write_word(ps.p, XWORD{XIS::PUSH})
				)
			) &&
			xasm_match         (ps.p, xtoken::OPERATOR_ENCLOSE_BRACE_L)      &&
			xasm_try_statements(new_state(xtoken::OPERATOR_ENCLOSE_BRACE_R)) &&
			xasm_match         (ps.p, xtoken::OPERATOR_ENCLOSE_BRACE_R)      &&
			(
				stack_size == 0 ||
				(
					xcc_write_word(ps.p, XWORD{XIS::PUT})   &&
					xcc_write_word(ps.p, XWORD{stack_size}) &&
					xcc_write_word(ps.p, XWORD{XIS::POP})
				)
			) &&
			xcc_pop_scope(ps.p)
		)
	) {
		
		return true;
	}
	return false;
}

static bool xasm_try_directive_bin(xcc_parser_state ps)
{
	U16 ip;
	if (
		manage_state(
			xasm_match            (ps.p, xtoken::KEYWORD_DIRECTIVE_BIN) &&
			xcc_write_word   (ps.p, XWORD{XIS::PUT})                    &&
			(ip = ps.p->out.size)                                       &&
			xcc_write_word   (ps.p, XWORD{0})                           &&
			xcc_write_word   (ps.p, XWORD{XIS::SKIP})                   &&
			xasm_try_emit_lit_list(new_state(xtoken::OPERATOR_STOP))    &&
			xasm_match            (ps.p, xtoken::OPERATOR_STOP)
		)
	) {
		ps.p->out.buffer[ip].u = ps.p->out.size - ip - 2;
		return true;
	}
	return false;
}

static bool xasm_try_directive_lit(xcc_parser_state ps)
{
	U16 val;
	token t;
	if (
		manage_state(
			xasm_match       (ps.p, xtoken::KEYWORD_DIRECTIVE_LIT)   &&
			xasm_match       (ps.p, token::ALIAS, &t)                &&
			xasm_match       (ps.p, xtoken::OPERATOR_COMMA)          &&
			xasm_try_lit_expr(new_state(xtoken::OPERATOR_STOP), val) &&
			xcc_add_lit (t, val, ps.p)                               &&
			xasm_match       (ps.p, xtoken::OPERATOR_STOP)
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_directive_stmt(xcc_parser_state ps)
{
	if (
		manage_state(
			xasm_match(ps.p, xtoken::OPERATOR_DIRECTIVE_DOLLAR) &&
			(
				xasm_try_directive_scope(new_state(ps.end)) ||
				xasm_try_directive_bin  (new_state(ps.end)) ||
				xasm_try_directive_lit  (new_state(ps.end))
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_redir_param(xcc_parser_state ps)
{
	if (
		manage_state(
			xasm_match         (ps.p, xtoken::OPERATOR_DIRECTIVE_AT) &&
			xasm_try_param     (new_state(ps.end))                   &&
			xcc_write_word(ps.p, XWORD{XIS::AT})
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_put_var(xcc_parser_state ps)
{
	token t;
	xcc_symbol *sym;
	bool skip_redir = xasm_match(ps.p, xtoken::OPERATOR_DIRECTIVE_ADDR);
	bool rs;
	if (
		manage_state(
			xasm_try_alias(new_state(ps.end), t, rs)       &&
			(sym = xcc_find_var(t.text, ps.p, rs)) != NULL &&
			xcc_write_rel(ps.p, sym)                       &&
			(
				skip_redir ||
				xcc_write_word(ps.p, XWORD{XIS::AT})
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_put_lit(xcc_parser_state ps)
{
	U16 lit = 0;
	if (
		manage_state(
			xasm_try_lit_expr  (new_state(ps.end), lit) &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})       &&
			xcc_write_word(ps.p, XWORD{lit})
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_opt_index(xcc_parser_state ps)
{
	const unsigned p = xasm_peek(ps.p).user_type;
	if (
		manage_state(
			(p == ps.end || p == xtoken::OPERATOR_COMMA) ||
			(
				xasm_match         (ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_L)      &&
				xasm_try_put_lit   (new_state(xtoken::OPERATOR_ENCLOSE_BRACKET_R)) &&
				xcc_write_word(ps.p, XWORD{XIS::ADD})                              &&
				xcc_write_word(ps.p, XWORD{XIS::AT})                               &&
				xasm_match         (ps.p, xtoken::OPERATOR_ENCLOSE_BRACKET_R)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_put_reg(xcc_parser_state ps)
{
	if (
		manage_state(
			xasm_match(ps.p, xtoken::OPERATOR_DIRECTIVE_DOLLAR) &&
			(
				(
					xasm_match         (ps.p, xtoken::KEYWORD_DIRECTIVE_HERE)  &&
					xcc_write_word(ps.p, XWORD{XIS::PUTI})
				) ||
				(
					xasm_match         (ps.p, xtoken::KEYWORD_DIRECTIVE_TOP)   &&
					xcc_write_word(ps.p, XWORD{XIS::PUTS})
				) ||
				(
					xasm_match         (ps.p, xtoken::KEYWORD_DIRECTIVE_FRAME) &&
					xcc_write_word(ps.p, XWORD{XIS::PUT})                      &&
					xcc_write_word(ps.p, XWORD{0})                             &&
					xcc_write_word(ps.p, XWORD{XIS::RLC})
				) ||
				(
					xasm_match         (ps.p, xtoken::KEYWORD_DIRECTIVE_BASE)  &&
					xcc_write_word(ps.p, XWORD{XIS::PUT})                      &&
					xcc_write_word(ps.p, XWORD{0})                             &&
					xcc_write_word(ps.p, XWORD{XIS::RLB})
				) ||
				(
					xasm_match         (ps.p, xtoken::KEYWORD_DIRECTIVE_ENTRY) &&
					xcc_write_word(ps.p, XWORD{XIS::PUT})                      &&
					xcc_write_word(ps.p, XWORD{0})                             &&
					xcc_write_word(ps.p, XWORD{XIS::RLA})
				) ||
				(
					xasm_match         (ps.p, xtoken::KEYWORD_DIRECTIVE_ERR)   &&
					xcc_write_word(ps.p, XWORD{XIS::ERR})
				)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool emit_empty_symbol_storage(xcc_parser *p, const xcc_symbol *sym)
{
	if (sym->storage != xcc_symbol::STORAGE_PARAM && sym->storage != xcc_symbol::STORAGE_LIT) {
		return 
			(sym->storage == xcc_symbol::STORAGE_STATIC ? xcc_write_word(p, XWORD{XIS::BIN}) : xcc_write_word(p, XWORD{XIS::PUT})) &&
			xcc_write_word(p, XWORD{0});
	}
	return true;
}

static bool xasm_try_put_lbl(xcc_parser_state ps)
{
	token t;
	xcc_symbol *sym;
	if (
		manage_state(
			xasm_match(ps.p, xtoken::OPERATOR_ARITHMETIC_MOD)  &&
			xasm_match(ps.p, token::ALIAS, &t)                 &&
			(
				(sym = xcc_find_lbl(t.text, ps.p)) != NULL ||
				(
					(sym = xcc_add_lbl(t, ps.p)) != NULL   &&
					emit_empty_symbol_storage(ps.p, sym)   &&
					(sym->link = ps.p->out.size - 1)       &&
					xcc_write_word(ps.p, XWORD{XIS::RLA})
				)
			)                                              &&
			xcc_write_rel (ps.p, sym)                      &&
			xcc_write_word(ps.p, XWORD{XIS::AT})
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_param(xcc_parser_state ps)
{
	if (
		manage_state(
			(
				xasm_try_redir_param(new_state(ps.end)) ||
				xasm_try_put_var    (new_state(ps.end)) ||
				xasm_try_put_lit    (new_state(ps.end)) ||
				xasm_try_put_reg    (new_state(ps.end)) ||
				xasm_try_put_lbl    (new_state(ps.end))
			) &&
			xasm_try_opt_index(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_redir_lparam(xcc_parser_state ps)
{
	if (
		manage_state(
			xasm_match         (ps.p, xtoken::OPERATOR_DIRECTIVE_AT) &&
			xasm_try_lparam    (new_state(ps.end))                   &&
			xcc_write_word(ps.p, XWORD{XIS::AT})
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_put_lvar(xcc_parser_state ps)
{
	token t;
	xcc_symbol *sym;
	bool rs;
	if (
		manage_state(
			xasm_try_alias(new_state(ps.end), t, rs)       &&
			(sym = xcc_find_var(t.text, ps.p, rs)) != NULL &&
			xcc_write_rel(ps.p, sym)
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_lparam(xcc_parser_state ps)
{
	if (
		manage_state(
			(
				xasm_try_redir_lparam(new_state(ps.end)) ||
				xasm_try_put_lvar    (new_state(ps.end)) ||
				xasm_try_put_lit     (new_state(ps.end))
			) &&
			xasm_try_opt_index(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_put_param(xcc_parser_state ps)
{
	if (
		manage_state(
			xasm_try_param(new_state(ps.end)) &&
			(
				!xasm_match       (ps.p, xtoken::OPERATOR_COMMA) ||
				xasm_try_put_param(new_state(ps.end))
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_instruction_put(xcc_parser_state ps)
{
	if (
		manage_state(
			xasm_match        (ps.p, XIS::PUT) &&
			xasm_try_put_param(new_state(xtoken::OPERATOR_STOP))
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_mov_param(xcc_parser_state ps)
{
	if (
		manage_state(
			xasm_try_lparam    (new_state(ps.end)) &&
			xcc_write_word(ps.p, XWORD{XIS::MOVU})
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_toss_param(xcc_parser_state ps)
{
	if (
		manage_state(
			xasm_match         (ps.p, xtoken::OPERATOR_ARITHMETIC_MUL) &&
			xcc_write_word(ps.p, XWORD{XIS::TOSS})
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_repeat_mov_param(xcc_parser_state ps)
{
	if (
		manage_state(
			(
				xasm_try_mov_param (new_state(ps.end))  ||
				xasm_try_toss_param(new_state(ps.end))
			) &&
			(
				!xasm_match              (ps.p, xtoken::OPERATOR_COMMA) ||
				xasm_try_repeat_mov_param(new_state(ps.end))
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_instruction_mov(xcc_parser_state ps)
{
	if (
		manage_state(
			xasm_match(ps.p, XIS::MOVU) &&
			(
				(
					xasm_peek(ps.p).user_type == ps.end &&
					xcc_write_word(ps.p, XWORD{XIS::MOVU})
				) ||
				xasm_try_repeat_mov_param(new_state(xtoken::OPERATOR_STOP))
			)
		)
	 ) {
		return true;
	}
	return false;
}

static bool xasm_try_instruction_all(xcc_parser_state ps)
{
	token i = xasm_peek(ps.p);
	if (
		manage_state(
			i.type      == token::KEYWORD                  &&
			i.user_type != xtoken::KEYWORD_DIRECTIVE_HERE  &&
			i.user_type != xtoken::KEYWORD_DIRECTIVE_FRAME &&
			i.user_type != xtoken::KEYWORD_DIRECTIVE_BASE  &&
			i.user_type != xtoken::KEYWORD_DIRECTIVE_ENTRY &&
			i.user_type != xtoken::KEYWORD_DIRECTIVE_TOP   &&
			i.user_type != xtoken::KEYWORD_DIRECTIVE_ERR   &&
			xasm_match    (ps.p, i.user_type)              &&
			xcc_write_word(ps.p, XWORD{U16(i.user_type)})
		)
	) {
		return true;
	}
	set_error(ps.p, i, xcc_error::UNDEF);
	return false;
}

static bool xasm_try_instruction_set(xcc_parser_state ps)
{

	if (
		manage_state(
			xasm_match         (ps.p, xtoken::KEYWORD_INSTRUCTION_SET) &&
			xasm_try_lparam    (new_state(xtoken::OPERATOR_COMMA))     &&
			xasm_match         (ps.p, xtoken::OPERATOR_COMMA)          &&
			xasm_try_param     (new_state(xtoken::OPERATOR_STOP))      &&
			xcc_write_word(ps.p, XWORD{XIS::MOVD})
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_instruction_stmt(xcc_parser_state ps)
{
	if (
		manage_state(
			(
				xasm_try_instruction_put(new_state(xtoken::OPERATOR_STOP)) ||
				xasm_try_instruction_mov(new_state(xtoken::OPERATOR_STOP)) ||
				xasm_try_instruction_set(new_state(xtoken::OPERATOR_STOP)) ||
				xasm_try_instruction_all(new_state(xtoken::OPERATOR_STOP))
			) &&
			xasm_match(ps.p, xtoken::OPERATOR_STOP)
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_emit_lit_list(xcc_parser_state ps)
{
	U16 lit = 0;
	while (
		manage_state(
			xasm_try_lit_expr(new_state(ps.end), lit) &&
			xcc_write_word(ps.p, XWORD{lit})
		)
	) {
		if (xasm_peek(ps.p).user_type == ps.end) {
			return true;
		} else if (!xasm_match(ps.p, xtoken::OPERATOR_COMMA)) {
			break;
		}
		lit = 0;
	}
	return false;
}

static bool xasm_try_lbl_def_stmt(xcc_parser_state ps)
{
	token t;
	xcc_symbol *sym;
	if (
		manage_state(
			xasm_match(ps.p, xtoken::OPERATOR_ARITHMETIC_MOD)   &&
			xasm_match(ps.p, token::ALIAS, &t)                  &&
			xasm_match(ps.p, xtoken::OPERATOR_COLON)            &&
			(
				(sym = xcc_find_lbl(t.text, ps.p)) != NULL ||
				(
					(sym = xcc_add_lbl(t, ps.p)) != NULL   &&
					emit_empty_symbol_storage(ps.p, sym)   &&
					(sym->link = ps.p->out.size - 1)       &&
					xcc_write_word(ps.p, XWORD{XIS::RLA})
				)
			 ) &&
			(sym->link == 0 || (ps.p->out.buffer[sym->link].u = ps.p->out.size))
		)
	) {
		sym->link = 0;
		return true;
	}
	return false;
}

static bool xasm_try_statement(xcc_parser_state ps)
{
	if (
		manage_state(
			xasm_match               (ps.p, xtoken::OPERATOR_STOP) ||
			xasm_try_directive_stmt  (new_state(ps.end))           ||
			xasm_try_lbl_def_stmt    (new_state(ps.end))           ||
			xasm_try_instruction_stmt(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xasm_try_statements(xcc_parser_state ps)
{
	while (xasm_peek(ps.p).user_type != ps.end) {
		if (
			!manage_state(
				xasm_try_statement(new_state(ps.end))
			)
		) {
			return false;
		}
	}
	return true;
}

static bool xasm_try_program(xcc_parser_state ps)
{
	if (
		manage_state(
			xcc_write_word(ps.p, XWORD{XIS::SVB})  &&
			xasm_try_statements(new_state(ps.end)) &&
			xcc_write_word(ps.p, XWORD{XIS::LDB})  &&
			xcc_write_word(ps.p, XWORD{XIS::HALT})
		)
	) {
		return true;
	}
	return false;
}

xcc_out xasm(lexer l, xcc_binary memory, const U16 sym_capacity, const unsigned file_capacity)
{
	xcc_symbol       sym_mem[sym_capacity];
	xcc_filesum      sum_mem[file_capacity];
	xcc_parser       p  = xcc_init_parser(l, memory, sym_mem, sym_capacity, sum_mem, file_capacity);
	xcc_parser_state ps = xcc_new_state(&p, NULL, token::STOP_EOF, 0, 0, 0);
	if (manage_state(xasm_try_program(new_state(ps.end)))) {
		return xcc_out{ p.in, p.out, p.max, 0, p.error };
	}
	set_error(ps.p, p.max, xcc_error::UNEXPECTED);
	return xcc_out{ p.in, p.out, p.max, 1, p.error };
}

bool xasm_inline(xcc_parser_state ps)
{
	if (
		manage_state(
			xasm_try_statement(new_state(ps.end))
		)
	) {
		return true;
	}
	set_error(ps.p, ps.p->max, xcc_error::UNEXPECTED);
	return false;
}

#undef new_state
#undef manage_state
#undef set_error

xcc_out xb(lexer l, const chars::view &std_lib_path, xcc_binary mem, const U16 sym_capacity = XCC_DEFAULT_SYM_CAPACITY, const unsigned file_capacity = XCC_DEFAULT_FILE_CAPACITY);
xcc_out xb(const chars::view *source_files, U16 num_source_files, const chars::view &std_lib_path, xcc_binary mem, const U16 sym_capacity = XCC_DEFAULT_SYM_CAPACITY, const unsigned file_capacity = XCC_DEFAULT_FILE_CAPACITY);

#define new_state(end) xcc_new_state(ps, end, ps.break_ip, ps.continue_ip, ps.loop_scope)
#define manage_state(success) xcc_manage_state(ps, ps.p->error.code == xcc_error::NONE && (success))
#define set_error(p, t, code) xcc_set_error(p, t, code, p->file, __FILE__, __LINE__)

struct xbtoken
{
	enum tokentype
	{
		KEYWORD_TYPE_AUTO,
		KEYWORD_TYPE_CONST,
		KEYWORD_TYPE_STATIC,
		KEYWORD_TYPE_VOID,
		KEYWORD_TYPE_UNSIGNED,
		KEYWORD_TYPE_SIGNED,
		KEYWORD_CONTROL_RETURN,
		KEYWORD_CONTROL_IF,
		KEYWORD_CONTROL_ELSE,
		KEYWORD_CONTROL_WHILE,
		KEYWORD_CONTROL_BREAK,
		KEYWORD_CONTROL_CONTINUE,
		KEYWORD_INTRINSIC_ASM,
		KEYWORD_NAMESPACE,
		KEYWORD_INCLUDE,
		KEYWORD_ENUM,
		OPERATOR_ARITHMETIC_ADD,
		OPERATOR_ARITHMETIC_SUB,
		OPERATOR_ARITHMETIC_MUL,
		OPERATOR_ARITHMETIC_DIV,
		OPERATOR_ARITHMETIC_MOD,
		OPERATOR_ARITHMETIC_INC,
		OPERATOR_ARITHMETIC_DEC,
		OPERATOR_ASSIGNMENT_SET,
		OPERATOR_ARITHMETIC_ASSIGNMENT_ADD,
		OPERATOR_ARITHMETIC_ASSIGNMENT_SUB,
		OPERATOR_ARITHMETIC_ASSIGNMENT_MUL,
		OPERATOR_ARITHMETIC_ASSIGNMENT_DIV,
		OPERATOR_ARITHMETIC_ASSIGNMENT_MOD,
		OPERATOR_BITWISE_ASSIGNMENT_AND,
		OPERATOR_BITWISE_ASSIGNMENT_OR,
		OPERATOR_BITWISE_ASSIGNMENT_XOR,
		OPERATOR_BITWISE_ASSIGNMENT_LSHIFT,
		OPERATOR_BITWISE_ASSIGNMENT_RSHIFT,
		OPERATOR_ENCLOSE_PARENTHESIS_L,
		OPERATOR_ENCLOSE_PARENTHESIS_R,
		OPERATOR_ENCLOSE_BRACKET_L,
		OPERATOR_ENCLOSE_BRACKET_R,
		OPERATOR_ENCLOSE_BRACE_L,
		OPERATOR_ENCLOSE_BRACE_R,
		OPERATOR_ENCLOSE_SINGLEQUOTE,
		OPERATOR_ENCLOSE_DOUBLEQUOTE,
		OPERATOR_MACRO,
		OPERATOR_LOGICAL_LESS,
		OPERATOR_LOGICAL_LESSEQUAL,
		OPERATOR_LOGICAL_GREATER,
		OPERATOR_LOGICAL_GREATEREQUAL,
		OPERATOR_LOGICAL_EQUAL,
		OPERATOR_LOGICAL_NOTEQUAL,
		OPERATOR_LOGICAL_AND,
		OPERATOR_LOGICAL_OR,
		OPERATOR_LOGICAL_NOT,
		OPERATOR_BITWISE_AND,
		OPERATOR_BITWISE_OR,
		OPERATOR_BITWISE_XOR,
		OPERATOR_BITWISE_NOT,
		OPERATOR_BITWISE_LSHIFT,
		OPERATOR_BITWISE_RSHIFT,
		OPERATOR_SEMICOLON,
		OPERATOR_COLON,
		OPERATOR_COMMA,
		OPERATOR_HASH,
		OPERATOR_REVERSE_SEARCH,
		OPERATOR_SIZEOF,
		LITERAL_INT
	};
};

const signed XB_TOKEN_COUNT = 68;
const token XB_TOKENS[XB_TOKEN_COUNT] = {
	new_keyword ("return",                  6, xbtoken::KEYWORD_CONTROL_RETURN),
	new_keyword ("if",                      2, xbtoken::KEYWORD_CONTROL_IF),
	new_keyword ("else",                    4, xbtoken::KEYWORD_CONTROL_ELSE),
	new_keyword ("while",                   5, xbtoken::KEYWORD_CONTROL_WHILE),
	new_keyword ("break",                   5, xbtoken::KEYWORD_CONTROL_BREAK),
	new_keyword ("continue",                8, xbtoken::KEYWORD_CONTROL_CONTINUE),
	new_keyword ("asm",                     3, xbtoken::KEYWORD_INTRINSIC_ASM),
	new_keyword ("auto",                    4, xbtoken::KEYWORD_TYPE_AUTO),
	new_keyword ("const",                   5, xbtoken::KEYWORD_TYPE_CONST),
	new_keyword ("static",                  6, xbtoken::KEYWORD_TYPE_STATIC),
	new_keyword ("void",                    4, xbtoken::KEYWORD_TYPE_VOID),
	new_keyword ("unsigned",                8, xbtoken::KEYWORD_TYPE_SIGNED),
	new_keyword ("signed",                  6, xbtoken::KEYWORD_TYPE_SIGNED),
	new_keyword ("namespace",               9, xbtoken::KEYWORD_NAMESPACE),
	new_keyword ("include",                 7, xbtoken::KEYWORD_INCLUDE),
	new_keyword ("enum",                    4, xbtoken::KEYWORD_ENUM),
	new_keyword ("sizeof",                  6, xbtoken::OPERATOR_SIZEOF),
	new_operator("#",                       1, xbtoken::OPERATOR_HASH),
	new_operator("+=",                      2, xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_ADD),
	new_operator("-=",                      2, xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_SUB),
	new_operator("*=",                      2, xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_MUL),
	new_operator("/=",                      2, xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_DIV),
	new_operator("%=",                      2, xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_MOD),
	new_operator("&=",                      2, xbtoken::OPERATOR_BITWISE_ASSIGNMENT_AND),
	new_operator("|=",                      2, xbtoken::OPERATOR_BITWISE_ASSIGNMENT_OR),
	new_operator("^=",                      2, xbtoken::OPERATOR_BITWISE_ASSIGNMENT_XOR),
	new_operator("<<=",                     3, xbtoken::OPERATOR_BITWISE_ASSIGNMENT_LSHIFT),
	new_operator(">>=",                     3, xbtoken::OPERATOR_BITWISE_ASSIGNMENT_RSHIFT),
	new_operator("<=",                      2, xbtoken::OPERATOR_LOGICAL_LESSEQUAL),
	new_operator(">=",                      2, xbtoken::OPERATOR_LOGICAL_GREATEREQUAL),
	new_operator("==",                      2, xbtoken::OPERATOR_LOGICAL_EQUAL),
	new_operator("!=",                      2, xbtoken::OPERATOR_LOGICAL_NOTEQUAL),
	new_operator("&&",                      2, xbtoken::OPERATOR_LOGICAL_AND),
	new_operator("||",                      2, xbtoken::OPERATOR_LOGICAL_OR),
	new_operator("<<",                      2, xbtoken::OPERATOR_BITWISE_LSHIFT),
	new_operator(">>",                      2, xbtoken::OPERATOR_BITWISE_RSHIFT),
	new_operator("++",                      2, xbtoken::OPERATOR_ARITHMETIC_INC),
	new_operator("--",                      2, xbtoken::OPERATOR_ARITHMETIC_DEC),
	new_operator("::",                      2, xbtoken::OPERATOR_REVERSE_SEARCH),
	new_comment ("//",                      2),
	new_operator("!",                       1, xbtoken::OPERATOR_LOGICAL_NOT),
	new_operator("#",                       1, xbtoken::OPERATOR_MACRO),
	new_operator("\"",                      1, xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE),
	new_operator("\'",                      1, xbtoken::OPERATOR_ENCLOSE_SINGLEQUOTE),
	new_operator("(",                       1, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L),
	new_operator(")",                       1, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R),
	new_operator("{",                       1, xbtoken::OPERATOR_ENCLOSE_BRACE_L),
	new_operator("}",                       1, xbtoken::OPERATOR_ENCLOSE_BRACE_R),
	new_operator("[",                       1, xbtoken::OPERATOR_ENCLOSE_BRACKET_L),
	new_operator("]",                       1, xbtoken::OPERATOR_ENCLOSE_BRACKET_R),
	new_operator("+",                       1, xbtoken::OPERATOR_ARITHMETIC_ADD),
	new_operator("-",                       1, xbtoken::OPERATOR_ARITHMETIC_SUB),
	new_operator("*",                       1, xbtoken::OPERATOR_ARITHMETIC_MUL),
	new_operator("/",                       1, xbtoken::OPERATOR_ARITHMETIC_DIV),
	new_operator("%",                       1, xbtoken::OPERATOR_ARITHMETIC_MOD),
	new_operator("=",                       1, xbtoken::OPERATOR_ASSIGNMENT_SET),
	new_operator(",",                       1, xbtoken::OPERATOR_COMMA),
	new_operator(";",                       1, xbtoken::OPERATOR_SEMICOLON),
	new_operator(":",                       1, xbtoken::OPERATOR_COLON),
	new_operator("<",                       1, xbtoken::OPERATOR_LOGICAL_LESS),
	new_operator(">",                       1, xbtoken::OPERATOR_LOGICAL_GREATER),
	new_operator("&",                       1, xbtoken::OPERATOR_BITWISE_AND),
	new_operator("|",                       1, xbtoken::OPERATOR_BITWISE_OR),
	new_operator("^",                       1, xbtoken::OPERATOR_BITWISE_XOR),
	new_operator("~",                       1, xbtoken::OPERATOR_BITWISE_NOT),
	new_alias   ("[a-zA-Z_][a-zA-Z0-9_]*", 22, token::ALIAS),
	new_literal ("[0-9]+",                  6, xbtoken::LITERAL_INT),
	new_literal ("0[xX][0-9a-fA-F]+",      17, xbtoken::LITERAL_INT, hex2u)
};

token xblex(lexer *l)
{
	return lex(l, XB_TOKENS, XB_TOKEN_COUNT);
}

token xblex1(lexer *l)
{
	return chlex(l);
}

static bool emit_pop_scope(xcc_parser *p)
{
	const U16 lsp = xcc_top_scope_stack_size(p);
	return
		(
			lsp == 0 ||
			(
				xcc_write_word(p, XWORD{XIS::PUT}) &&
				xcc_write_word(p, XWORD{lsp})      &&
				xcc_write_word(p, XWORD{XIS::POP})
			)
		) &&
		xcc_pop_scope(p);
}

static token xb_peek(xcc_parser *p)
{
	return xcc_peek(p, xblex);
}

static token xb_peek1(xcc_parser *p)
{
	return xcc_peek(p, xblex1);
}

static bool xb_match(xcc_parser *p, unsigned type, token *out = NULL)
{
	return xcc_match(p, type, out, xblex);
}

static bool xb_match1(xcc_parser *p, unsigned type, token *out = NULL)
{
	return xcc_match(p, type, out, xblex1);
}

static bool until_end(xcc_parser_state ps, bool (*xb_try_fn)(xcc_parser_state))
{
	token t;
	while ((t = xb_peek(ps.p)).user_type != ps.end && t.user_type != token::STOP_EOF) {
		if (
			!manage_state(
				xb_try_fn(new_state(ps.end))
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xb_try_read_lit(xcc_parser_state ps, type_t &result);

static bool xb_try_put_lit(xcc_parser_state ps)
{
	U16 result = 0;
	if (
		manage_state(
			xb_try_read_lit(new_state(ps.end), result) &&
			xcc_write_word (ps.p, XWORD{XIS::PUT})     &&
			xcc_write_word (ps.p, XWORD{result})
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_alias(xcc_parser_state ps, token &t, bool &reverse_search)
{
	reverse_search = false;
	if (
		manage_state(
			(
				reverse_search = xb_match(ps.p, xbtoken::OPERATOR_REVERSE_SEARCH) &&
				xb_match(ps.p, token::ALIAS, &t)
			) ||
			xb_match(ps.p, token::ALIAS, &t)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_put_var_addr(xcc_parser_state ps)
{
	token t;
	xcc_symbol *sym;
	bool rs;
	if (
		manage_state(
			xb_try_alias(new_state(ps.end), t, rs)         &&
			(sym = xcc_find_var(t.text, ps.p, rs)) != NULL &&
			xcc_write_rel(ps.p, sym)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_put_fn_addr(xcc_parser_state ps)
{
	token t;
	xcc_symbol *sym;
	bool rs;
	if (
		manage_state(
			xb_try_alias(new_state(ps.end), t, rs)        &&
			(sym = xcc_find_fn(t.text, ps.p, rs)) != NULL &&
			xcc_write_rel(ps.p, sym)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_rval(xcc_parser_state ps);

static bool xb_try_redir_val(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_match      (ps.p, xbtoken::OPERATOR_ARITHMETIC_MUL) &&
			xb_try_rval   (new_state(ps.end))                      &&
			xcc_write_word(ps.p, XWORD{XIS::AT})
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_put_var(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_redir_val(new_state(ps.end)) ||
			(
				xb_try_put_var_addr(new_state(ps.end)) &&
				xcc_write_word(ps.p, XWORD{XIS::AT})
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_put_fn(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_redir_val(new_state(ps.end)) ||
			(
				xb_try_put_fn_addr(new_state(ps.end)) &&
				xcc_write_word(ps.p, XWORD{XIS::AT})
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_post_incdec_var(xcc_parser_state ps)
{
	token t, op;
	xcc_symbol *sym;
	bool rs;
	if (
		manage_state(
			xb_try_alias(new_state(ps.end), t, rs)         &&
			(sym = xcc_find_var(t.text, ps.p, rs)) != NULL &&
			(
				xb_match(ps.p, xbtoken::OPERATOR_ARITHMETIC_INC, &op) ||
				xb_match(ps.p, xbtoken::OPERATOR_ARITHMETIC_DEC, &op)
			)                                              &&
			xcc_write_rel (ps.p, sym)                      &&
			xcc_write_word(ps.p, XWORD{XIS::AT})           &&
			xcc_write_word(ps.p, XWORD{XIS::DUP})          &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})          &&
			xcc_write_word(ps.p, XWORD{1})                 &&
			(
				op.user_type == xbtoken::OPERATOR_ARITHMETIC_INC ?
					xcc_write_word(ps.p, XWORD{XIS::ADD}) :
					xcc_write_word(ps.p, XWORD{XIS::SUB})
			)                                             &&
			xcc_write_rel (ps.p, sym)                     &&
			xcc_write_word(ps.p, XWORD{XIS::MOVU})
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_pre_incdec_var(xcc_parser_state ps)
{
	token t, op;
	xcc_symbol *sym;
	bool rs;
	if (
		manage_state(
			(
				xb_match(ps.p, xbtoken::OPERATOR_ARITHMETIC_INC, &op) ||
				xb_match(ps.p, xbtoken::OPERATOR_ARITHMETIC_DEC, &op)
			)                                                 &&
			xb_try_alias(new_state(ps.end), t, rs)            &&
			(sym = xcc_find_var(t.text, ps.p, rs)) != NULL    &&
			xcc_write_rel (ps.p, sym)                         &&
			xcc_write_word(ps.p, XWORD{XIS::AT})              &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})             &&
			xcc_write_word(ps.p, XWORD{1})                    &&
			(
				op.user_type == xbtoken::OPERATOR_ARITHMETIC_INC ?
					xcc_write_word(ps.p, XWORD{XIS::ADD}) :
					xcc_write_word(ps.p, XWORD{XIS::SUB})
			)                                              &&
			xcc_write_word(ps.p, XWORD{XIS::DUP})          &&
			xcc_write_rel (ps.p, sym)                      &&
			xcc_write_word(ps.p, XWORD{XIS::MOVU})
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_expr(xcc_parser_state ps);

static bool xb_try_put_fn_param(xcc_parser_state ps, U16 *param_count)
{
	if (
		manage_state(
			xb_try_expr(new_state(ps.end))
		)
	) {
		if (param_count != NULL) {
			++(*param_count);
		}
		return true;
	}
	return false;
}

static bool xb_try_put_fn_params(xcc_parser_state ps, U16 *param_count)
{
	if (
		manage_state(
			xb_try_put_fn_param(new_state(ps.end), param_count) &&
			(
				xb_peek(ps.p).user_type == ps.end ||
				(
					xb_match(ps.p, xbtoken::OPERATOR_COMMA) &&
					xb_try_put_fn_params(new_state(ps.end), param_count)
				)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_put_opt_fn_params(xcc_parser_state ps, const xcc_symbol *sym)
{
	U16 param_count = 0;
	if (
		manage_state(
			xb_peek(ps.p).user_type == ps.end ||
			xb_try_put_fn_params(new_state(ps.end), &param_count)
		)
	) {
		if (sym != NULL && sym->storage == xcc_symbol::STORAGE_FN && sym->param_count != param_count) {
			set_error(ps.p, sym->tok, xcc_error::VERIFY);
			return false;
		}
		return true;
	}
	return false;
}

static bool xb_try_call_fn_inline_arr(xcc_parser_state ps)
{
	return false;
}

static bool xb_try_call_fn(xcc_parser_state ps)
{
	if (!xb_try_call_fn_inline_arr(new_state(ps.end))) {
		xcc_symbol *sym;
		U16 off_index = 0;
		token t;
		U16 result = 0;
		bool rs;
		if (
			manage_state(
				xb_try_alias(new_state(ps.end), t, rs)                                            &&
				(
					(sym = xcc_find_symbol(t.text, ps.p, rs)) != NULL                             ||
					xb_try_read_lit(new_state(ps.end), result)
				)                                                                                 &&
				xb_match                (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)           &&
				xcc_write_word          (ps.p, XWORD{XIS::PUT})                                   &&
				xcc_write_word          (ps.p, XWORD{0})                                          &&
				xcc_write_word          (ps.p, XWORD{XIS::PUTI})                                  &&
				xcc_write_word          (ps.p, XWORD{XIS::PUT})                                   &&
				(off_index = ps.p->out.size)                                                      &&
				xcc_write_word          (ps.p, XWORD{0})                                          &&
				xcc_write_word          (ps.p, XWORD{XIS::ADD})                                   &&
				xb_try_put_opt_fn_params(new_state(xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R), sym) &&
				xb_match                (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)           &&
				(ps.p->out.buffer[off_index].u = (ps.p->out.size - off_index) + 7)                &&
				(
					sym != NULL ?
						xcc_write_rel(ps.p, sym) :
						xcc_write_word(ps.p, XWORD{result})
				)                                                                                 &&
				xcc_write_word       (ps.p, XWORD{XIS::AT})                                       &&
				xcc_write_word       (ps.p, XWORD{XIS::JMP})
			)
		) {
			return true;
		}
		if (t.user_type == token::ALIAS && sym == NULL) {
			set_error(ps.p, t, xcc_error::UNDEF);
		}
	}
	return false;
}

template < typename type_t >
static bool xb_eval_operation(xcc_parser *p, unsigned user_type, type_t &l, type_t r)
{
	switch (user_type) {
	case xbtoken::OPERATOR_ARITHMETIC_ADD:       l += r;                                                                                 return true;
	case xbtoken::OPERATOR_ARITHMETIC_SUB:       l -= r;                                                                                 return true;
	case xbtoken::OPERATOR_ARITHMETIC_MUL:       l *= r;                                                                                 return true;
	case xbtoken::OPERATOR_ARITHMETIC_DIV:       if (r != 0) { l /= r; } else { l = 0xffff; set_error(p, p->in.last, xcc_error::ZERO); } return true;
	case xbtoken::OPERATOR_ARITHMETIC_MOD:       if (r != 0) { l %= r; } else { l = 0xffff; set_error(p, p->in.last, xcc_error::ZERO); } return true;
	case xbtoken::OPERATOR_BITWISE_AND:
	case xbtoken::OPERATOR_LOGICAL_AND:          l &= r;                                                                                 return true;
	case xbtoken::OPERATOR_BITWISE_OR:
	case xbtoken::OPERATOR_LOGICAL_OR:           l |= r;                                                                                 return true;
	case xbtoken::OPERATOR_BITWISE_XOR:          l ^= r;                                                                                 return true;
	case xbtoken::OPERATOR_BITWISE_NOT:          l = ~r;                                                                                 return true;
	case xbtoken::OPERATOR_BITWISE_LSHIFT:       l <<= r;                                                                                return true;
	case xbtoken::OPERATOR_BITWISE_RSHIFT:       l >>= r;                                                                                return true;
	case xbtoken::OPERATOR_LOGICAL_LESS:         l = (l < r);                                                                            return true;
	case xbtoken::OPERATOR_LOGICAL_LESSEQUAL:    l = (l <= r);                                                                           return true;
	case xbtoken::OPERATOR_LOGICAL_GREATER:      l = (l > r);                                                                            return true;
	case xbtoken::OPERATOR_LOGICAL_GREATEREQUAL: l = (l >= r);                                                                           return true;
	case xbtoken::OPERATOR_LOGICAL_EQUAL:        l = (l == r);                                                                           return true;
	case xbtoken::OPERATOR_LOGICAL_NOTEQUAL:     l = (l != r);                                                                           return true;
	case xbtoken::OPERATOR_LOGICAL_NOT:          l = !r;                                                                                 return true;
	}
	return false;
}

static bool xb_try_escape_char(xcc_parser_state ps, token &t)
{
	if (
		manage_state(
			xb_match1(ps.p, token::CHAR, &t)
		)
	) {
		switch (t.hash) {
		case '\\':               return true;
		case 't': t.hash = '\t'; return true;
		case 'r': t.hash = '\r'; return true;
		case 'a': t.hash = '\a'; return true;
		case 'n': t.hash = '\n'; return true;
		case 'b': t.hash = '\b'; return true;
		case 'f': t.hash = '\f'; return true;
		case '0': t.hash = '\0'; return true;
		}
		if (t.hash == ps.end) {
			switch (t.hash) {
			case '\'': return true;
			case '"':  return true;
			}
		}
	}
	return false;
}

static unsigned hexdig2dec(char hex)
{
	switch (hex) {
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		return hex - '0';
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
		return (hex - 'a') + 10;
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
		return (hex - 'A') + 10;
	}
	return 0;
}

static bool xb_try_encoded_char(xcc_parser_state ps, token &t)
{
	if (
		manage_state(
			xb_match1(ps.p, token::CHAR, &t) &&
			(
				(t.hash == '\\') ?
					xb_try_escape_char(new_state(ps.end), t) :
					true
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_ext_escape_char(xcc_parser_state ps, token &t)
{
	token hi, lo;
	if (
		manage_state(
			xb_match1(ps.p, token::CHAR, &t) &&
			t.hash == '#' &&
			xb_match1(ps.p, token::CHAR, &hi) &&
			xb_match1(ps.p, token::CHAR, &lo) &&
			xb_try_encoded_char(new_state(ps.end), t)
		)
	) {
		t.hash = (((hexdig2dec(hi.hash) << 4) + hexdig2dec(lo.hash)) << 8) + t.hash;
		return true;
	}
	return false;
}

static bool xb_try_ext_encoded_char(xcc_parser_state ps, token &t)
{
	if (
		manage_state(
			xb_match1(ps.p, token::CHAR, &t) &&
			(
				(t.hash == '\\') ?
					xb_try_ext_escape_char(new_state(ps.end), t) || xb_try_escape_char(new_state(ps.end), t) :
					true
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_read_char_lit(xcc_parser_state ps, token &t)
{
	if (
		manage_state(
			xb_match               (ps.p, xbtoken::OPERATOR_ENCLOSE_SINGLEQUOTE)         &&
			xb_try_ext_encoded_char(new_state(xbtoken::OPERATOR_ENCLOSE_SINGLEQUOTE), t) &&
			xb_match               (ps.p, xbtoken::OPERATOR_ENCLOSE_SINGLEQUOTE)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xb_try_lit_factor(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xb_try_read_lit(xcc_parser_state ps, type_t &result)
{
	token t;
	bool rs;
	if (xb_match(ps.p, xbtoken::LITERAL_INT, &t)) {
		result = t.hash;
		return true;
	} else if (xb_try_alias(new_state(ps.end), t, rs)) {
		const xcc_symbol *sym = xcc_find_lit(t.text, ps.p, rs);
		if (sym != NULL) {
			result = sym->data.u;
			return true;
		}
	} else if (xb_try_read_char_lit(new_state(ps.end), t)) {
		result = t.hash;
		return true;
	} else if (
		xb_match    (ps.p, xbtoken::OPERATOR_SIZEOF)                            &&
		xb_match    (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)             &&
		xb_try_alias(new_state(xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R), t, rs) &&
		xb_match    (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
	) {
		const xcc_symbol *sym = xcc_find_symbol(t.text, ps.p, rs);
		if (sym != NULL) {
			result = sym->size > 1 ? sym->size - 1 : sym->size;
			return true;
		}
	}
	return false;
}

template < typename type_t >
static bool xb_try_lit_opt_factor(xcc_parser_state ps, type_t &l)
{
	token t;
	while (xb_match(ps.p, xbtoken::OPERATOR_ARITHMETIC_MUL, &t) || xb_match(ps.p, xbtoken::OPERATOR_ARITHMETIC_DIV, &t) || xb_match(ps.p, xbtoken::OPERATOR_ARITHMETIC_MOD, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				xb_try_lit_factor(new_state(ps.end), r)    &&
				xb_eval_operation(ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xb_try_lit_rval(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xb_try_lit_uni_bnot_val(xcc_parser_state ps, type_t &l)
{
	token t;
	if (
		manage_state(
			xb_match         (ps.p, xbtoken::OPERATOR_BITWISE_NOT, &t) &&
			xb_try_lit_rval  (new_state(ps.end), l)                    &&
			xb_eval_operation(ps.p, t.user_type, l, l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xb_try_lit_uni_lnot_val(xcc_parser_state ps, type_t &l)
{
	token t;
	if (
		manage_state(
			xb_match         (ps.p, xbtoken::OPERATOR_LOGICAL_NOT, &t) &&
			xb_try_lit_rval  (new_state(ps.end), l)                    &&
			xb_eval_operation(ps.p, t.user_type, l, l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xb_try_lit_rval(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xb_try_read_lit(new_state(ps.end), l) ||
			(
				(
					xb_match       (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)         &&
					xb_try_lit_expr(new_state(xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R), l) &&
					xb_match       (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
				)
			)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xb_try_lit_uni_pos(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xb_match       (ps.p, xbtoken::OPERATOR_ARITHMETIC_ADD) &&
			xb_try_lit_rval(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xb_try_lit_uni_neg(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xb_match       (ps.p, xbtoken::OPERATOR_ARITHMETIC_SUB) &&
			xb_try_lit_rval(new_state(ps.end), l)
		)
	) {
		l = -l;
		return true;
	}
	return false;
}

template < typename type_t >
static bool xb_try_lit_factor(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xb_try_lit_uni_pos     (new_state(ps.end), l) ||
			xb_try_lit_uni_neg     (new_state(ps.end), l) ||
			xb_try_lit_uni_bnot_val(new_state(ps.end), l) ||
			xb_try_lit_uni_lnot_val(new_state(ps.end), l) ||
			xb_try_lit_rval        (new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xb_try_lit_term(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xb_try_lit_opt_term(xcc_parser_state ps, type_t &l)
{
	token t;
	while (xb_match(ps.p, xbtoken::OPERATOR_ARITHMETIC_ADD, &t) || xb_match(ps.p, xbtoken::OPERATOR_ARITHMETIC_SUB, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				xb_try_lit_term  (new_state(ps.end), r)    &&
				xb_eval_operation(ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xb_try_lit_term(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xb_try_lit_factor    (new_state(ps.end), l) &&
			xb_try_lit_opt_factor(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xb_try_lit_bitshift(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xb_try_lit_opt_bitshift(xcc_parser_state ps, type_t &l)
{
	token t;
	while (xb_match(ps.p, xbtoken::OPERATOR_BITWISE_LSHIFT, &t) || xb_match(ps.p, xbtoken::OPERATOR_BITWISE_RSHIFT, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				xb_try_lit_bitshift(new_state(ps.end), r)    &&
				xb_eval_operation  (ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xb_try_lit_bitshift(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xb_try_lit_term    (new_state(ps.end), l) &&
			xb_try_lit_opt_term(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xb_try_lit_less_greater(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xb_try_lit_opt_less_greater(xcc_parser_state ps, type_t &l)
{
	token t;
	while (xb_match(ps.p, xbtoken::OPERATOR_LOGICAL_LESS, &t) || xb_match(ps.p, xbtoken::OPERATOR_LOGICAL_LESSEQUAL, &t) || xb_match(ps.p, xbtoken::OPERATOR_LOGICAL_GREATER, &t) || xb_match(ps.p, xbtoken::OPERATOR_LOGICAL_GREATEREQUAL, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				xb_try_lit_less_greater(new_state(ps.end), r)    &&
				xb_eval_operation      (ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xb_try_lit_less_greater(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xb_try_lit_bitshift    (new_state(ps.end), l) &&
			xb_try_lit_opt_bitshift(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xb_try_lit_equality(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xb_try_lit_opt_equality(xcc_parser_state ps, type_t &l)
{
	token t;
	while (xb_match(ps.p, xbtoken::OPERATOR_LOGICAL_EQUAL, &t) || xb_match(ps.p, xbtoken::OPERATOR_LOGICAL_NOTEQUAL, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				xb_try_lit_equality(new_state(ps.end), r)    &&
				xb_eval_operation  (ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xb_try_lit_equality(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xb_try_lit_less_greater    (new_state(ps.end), l) &&
			xb_try_lit_opt_less_greater(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xb_try_lit_and(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xb_try_lit_opt_and(xcc_parser_state ps, type_t &l)
{
	token t;
	while (xb_match(ps.p, xbtoken::OPERATOR_BITWISE_AND, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				xb_try_lit_and   (new_state(ps.end), r)    &&
				xb_eval_operation(ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xb_try_lit_and(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xb_try_lit_equality    (new_state(ps.end), l) &&
			xb_try_lit_opt_equality(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xb_try_lit_xor(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xb_try_lit_opt_xor(xcc_parser_state ps, type_t &l)
{
	token t;
	while (xb_match(ps.p, xbtoken::OPERATOR_BITWISE_XOR, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				xb_try_lit_xor   (new_state(ps.end), r)    &&
				xb_eval_operation(ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xb_try_lit_xor(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xb_try_lit_and    (new_state(ps.end), l) &&
			xb_try_lit_opt_and(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xb_try_lit_or(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xb_try_lit_opt_or(xcc_parser_state ps, type_t &l)
{
	token t;
	while (xb_match(ps.p, xbtoken::OPERATOR_BITWISE_OR, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				xb_try_lit_or    (new_state(ps.end), r)    &&
				xb_eval_operation(ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xb_try_lit_or(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xb_try_lit_xor    (new_state(ps.end), l) &&
			xb_try_lit_opt_xor(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xb_try_lit_logical_and(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xb_try_lit_opt_logical_and(xcc_parser_state ps, type_t &l)
{
	token t;
	while (xb_match(ps.p, xbtoken::OPERATOR_LOGICAL_AND, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				xb_try_lit_logical_and(new_state(ps.end), r)    &&
				xb_eval_operation     (ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xb_try_lit_logical_and(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xb_try_lit_or    (new_state(ps.end), l) &&
			xb_try_lit_opt_or(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xb_try_lit_logical_or(xcc_parser_state ps, type_t &l);

template < typename type_t >
static bool xb_try_lit_opt_logical_or(xcc_parser_state ps, type_t &l)
{
	token t;
	while (xb_match(ps.p, xbtoken::OPERATOR_LOGICAL_OR, &t)) {
		type_t r = 0;
		if (
			!manage_state(
				xb_try_lit_logical_or(new_state(ps.end), r)    &&
				xb_eval_operation    (ps.p, t.user_type, l, r)
			)
		) {
			return false;
		}
	}
	return true;
}

template < typename type_t >
static bool xb_try_lit_logical_or(xcc_parser_state ps, type_t &l)
{
	if (
		manage_state(
			xb_try_lit_logical_and    (new_state(ps.end), l) &&
			xb_try_lit_opt_logical_and(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

template < typename type_t >
static bool xb_try_lit_expr(xcc_parser_state ps, type_t &l)
{

	if (
		manage_state(
			xb_try_lit_logical_or    (new_state(ps.end), l) &&
			xb_try_lit_opt_logical_or(new_state(ps.end), l)
		)
	) {
		return true;
	}
	return false;
}

static bool emit_operation(xcc_parser *p, unsigned user_type)
{
	switch (user_type) {
	case xbtoken::OPERATOR_ARITHMETIC_ADD:       return xcc_write_word(p, XWORD{XIS::ADD});
	case xbtoken::OPERATOR_ARITHMETIC_SUB:       return xcc_write_word(p, XWORD{XIS::SUB});
	case xbtoken::OPERATOR_ARITHMETIC_MUL:       return xcc_write_word(p, XWORD{XIS::MUL});
	case xbtoken::OPERATOR_ARITHMETIC_DIV:       return xcc_write_word(p, XWORD{XIS::DIV});
	case xbtoken::OPERATOR_ARITHMETIC_MOD:       return xcc_write_word(p, XWORD{XIS::MOD});
	case xbtoken::OPERATOR_BITWISE_AND:
	case xbtoken::OPERATOR_LOGICAL_AND:          return xcc_write_word(p, XWORD{XIS::AND});
	case xbtoken::OPERATOR_BITWISE_OR:
	case xbtoken::OPERATOR_LOGICAL_OR:           return xcc_write_word(p, XWORD{XIS::OR});
	case xbtoken::OPERATOR_BITWISE_XOR:          return xcc_write_word(p, XWORD{XIS::XOR});
	case xbtoken::OPERATOR_BITWISE_NOT:          return xcc_write_word(p, XWORD{XIS::NOT});
	case xbtoken::OPERATOR_BITWISE_LSHIFT:       return xcc_write_word(p, XWORD{XIS::LSH});
	case xbtoken::OPERATOR_BITWISE_RSHIFT:       return xcc_write_word(p, XWORD{XIS::RSH});
	case xbtoken::OPERATOR_LOGICAL_LESS:         return xcc_write_word(p, XWORD{XIS::LT});
	case xbtoken::OPERATOR_LOGICAL_LESSEQUAL:    return xcc_write_word(p, XWORD{XIS::LE});
	case xbtoken::OPERATOR_LOGICAL_GREATER:      return xcc_write_word(p, XWORD{XIS::GT});
	case xbtoken::OPERATOR_LOGICAL_GREATEREQUAL: return xcc_write_word(p, XWORD{XIS::LE});
	case xbtoken::OPERATOR_LOGICAL_EQUAL:        return xcc_write_word(p, XWORD{XIS::EQ});
	case xbtoken::OPERATOR_LOGICAL_NOTEQUAL:     return xcc_write_word(p, XWORD{XIS::NE});
	case xbtoken::OPERATOR_LOGICAL_NOT:
		return 
			xcc_write_word(p, XWORD{XIS::PUT}) &&
			xcc_write_word(p, XWORD{0})        &&
			xcc_write_word(p, XWORD{XIS::EQ})
		;
	}
	return false;
}

static bool xb_try_factor(xcc_parser_state ps);

static bool xb_try_opt_factor(xcc_parser_state ps)
{
	token t;
	while (xb_match(ps.p, xbtoken::OPERATOR_ARITHMETIC_MUL, &t) || xb_match(ps.p, xbtoken::OPERATOR_ARITHMETIC_DIV, &t) || xb_match(ps.p, xbtoken::OPERATOR_ARITHMETIC_MOD, &t)) {
		if (
			!manage_state(
				xb_try_factor (new_state(ps.end)) &&
				emit_operation(ps.p, t.user_type)
			)
		) {
			return false;
		}
	}
	return true;
}

static bool xb_try_index_src(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_put_var_addr(new_state(ps.end)) ||
			xb_try_put_lit     (new_state(ps.end)) ||
			(
				xb_match   (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)      &&
				xb_try_expr(new_state(xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
				xb_match   (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_opt_index(xcc_parser_state ps)
{
	if (xb_peek(ps.p).user_type != xbtoken::OPERATOR_ENCLOSE_BRACKET_L) {
		return true;
	}
	if (
		manage_state(
			xcc_write_word  (ps.p, XWORD{XIS::AT})                           &&
			xb_match        (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_L)      &&
			xb_try_expr     (new_state(xbtoken::OPERATOR_ENCLOSE_BRACKET_R)) &&
			xb_match        (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_R)      &&
			xcc_write_word  (ps.p, XWORD{XIS::ADD})                          &&
			xb_try_opt_index(new_state(ps.end))
		)
	) {
			return true;
	}
	return false;
}

static bool xb_try_put_index_addr(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_index_src(new_state(ps.end))                              &&
			xcc_write_word  (ps.p, XWORD{XIS::AT})                           &&
			xb_match        (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_L)      &&
			xb_try_expr     (new_state(xbtoken::OPERATOR_ENCLOSE_BRACKET_R)) &&
			xb_match        (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_R)      &&
			xcc_write_word  (ps.p, XWORD{XIS::ADD})                          &&
			xb_try_opt_index(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_put_index(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_put_index_addr(new_state(ps.end))    &&
			xcc_write_word    (ps.p, XWORD{XIS::AT})
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_uni_bnot_val(xcc_parser_state ps)
{
	token t;
	if (
		manage_state(
			xb_match      (ps.p, xbtoken::OPERATOR_BITWISE_NOT, &t) &&
			xb_try_rval   (new_state(ps.end))                       &&
			emit_operation(ps.p, t.user_type)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_uni_lnot_val(xcc_parser_state ps)
{
	token t;
	if (
		manage_state(
			xb_match         (ps.p, xbtoken::OPERATOR_LOGICAL_NOT, &t) &&
			xb_try_rval      (new_state(ps.end))                       &&
			emit_operation(ps.p, t.user_type)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_rval(xcc_parser_state ps)
{
	if (
		manage_state(
			
			xb_try_call_fn        (new_state(ps.end)) ||
			xb_try_put_lit        (new_state(ps.end)) ||
			xb_try_put_index      (new_state(ps.end)) ||
			xb_try_post_incdec_var(new_state(ps.end)) ||
			xb_try_put_var        (new_state(ps.end)) ||
			xb_try_put_fn         (new_state(ps.end)) ||
			(
				(
					xb_match   (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)      &&
					xb_try_expr(new_state(xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
					xb_match   (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)
				)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_uni_addr(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_match(ps.p, xbtoken::OPERATOR_BITWISE_AND) &&
			xb_try_put_var_addr(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_uni_pos(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_match(ps.p, xbtoken::OPERATOR_ARITHMETIC_ADD) &&
			xb_try_rval(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_uni_neg(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_match     (ps.p, xbtoken::OPERATOR_ARITHMETIC_SUB) &&
			xb_try_rval  (new_state(ps.end))                      &&
			xcc_write_word(ps.p, XWORD{XIS::INEG})
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_factor(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_uni_addr      (new_state(ps.end)) ||
			xb_try_uni_pos       (new_state(ps.end)) ||
			xb_try_uni_neg       (new_state(ps.end)) ||
			xb_try_uni_bnot_val  (new_state(ps.end)) ||
			xb_try_uni_lnot_val  (new_state(ps.end)) ||
			xb_try_pre_incdec_var(new_state(ps.end)) ||
			xb_try_rval          (new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_term(xcc_parser_state ps);

static bool xb_try_opt_term(xcc_parser_state ps)
{
	token t;
	while (xb_match(ps.p, xbtoken::OPERATOR_ARITHMETIC_ADD, &t) || xb_match(ps.p, xbtoken::OPERATOR_ARITHMETIC_SUB, &t)) {
		if (
			!manage_state(
				xb_try_term(new_state(ps.end)) &&
				emit_operation(ps.p, t.user_type)
			)
		) {
			return false;
		}
	}
	return true;
}

static bool xb_try_term(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_factor    (new_state(ps.end)) &&
			xb_try_opt_factor(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_bitshift(xcc_parser_state ps);

static bool xb_try_opt_bitshift(xcc_parser_state ps)
{
	token t;
	while (xb_match(ps.p, xbtoken::OPERATOR_BITWISE_LSHIFT, &t) || xb_match(ps.p, xbtoken::OPERATOR_BITWISE_RSHIFT, &t)) {
		if (
			!manage_state(
				xb_try_bitshift(new_state(ps.end)) &&
				emit_operation(ps.p, t.user_type)
			)
		) {
			return false;
		}
	}
	return true;
}

static bool xb_try_bitshift(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_term    (new_state(ps.end)) &&
			xb_try_opt_term(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_less_greater(xcc_parser_state ps);

static bool xb_try_opt_less_greater(xcc_parser_state ps)
{
	token t;
	while (xb_match(ps.p, xbtoken::OPERATOR_LOGICAL_LESS, &t) || xb_match(ps.p, xbtoken::OPERATOR_LOGICAL_LESSEQUAL, &t) || xb_match(ps.p, xbtoken::OPERATOR_LOGICAL_GREATER, &t) || xb_match(ps.p, xbtoken::OPERATOR_LOGICAL_GREATEREQUAL, &t)) {
		if (
			!manage_state(
				xb_try_less_greater(new_state(ps.end)) &&
				emit_operation     (ps.p, t.user_type)
			)
		) {
			return false;
		}
	}
	return true;
}

static bool xb_try_less_greater(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_bitshift    (new_state(ps.end)) &&
			xb_try_opt_bitshift(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_equality(xcc_parser_state ps);

static bool xb_try_opt_equality(xcc_parser_state ps)
{
	token t;
	while (xb_match(ps.p, xbtoken::OPERATOR_LOGICAL_EQUAL, &t) || xb_match(ps.p, xbtoken::OPERATOR_LOGICAL_NOTEQUAL, &t)) {
		if (
			!manage_state(
				xb_try_equality(new_state(ps.end)) &&
				emit_operation (ps.p, t.user_type)
			)
		) {
			return false;
		}
	}
	return true;
}

static bool xb_try_equality(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_less_greater    (new_state(ps.end)) &&
			xb_try_opt_less_greater(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_and(xcc_parser_state ps);

static bool xb_try_opt_and(xcc_parser_state ps)
{
	token t;
	while (xb_match(ps.p, xbtoken::OPERATOR_BITWISE_AND, &t)) {
		if (
			!manage_state(
				xb_try_and    (new_state(ps.end)) &&
				emit_operation(ps.p, t.user_type)
			)
		) {
			return false;
		}
	}
	return true;
}

static bool xb_try_and(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_equality    (new_state(ps.end)) &&
			xb_try_opt_equality(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_xor(xcc_parser_state ps);

static bool xb_try_opt_xor(xcc_parser_state ps)
{
	token t;
	while (xb_match(ps.p, xbtoken::OPERATOR_BITWISE_XOR, &t)) {
		if (
			!manage_state(
				xb_try_xor    (new_state(ps.end)) &&
				emit_operation(ps.p, t.user_type)
			)
		) {
			return false;
		}
	}
	return true;
}

static bool xb_try_xor(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_and    (new_state(ps.end)) &&
			xb_try_opt_and(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_or(xcc_parser_state ps);

static bool xb_try_opt_or(xcc_parser_state ps)
{
	token t;
	while (xb_match(ps.p, xbtoken::OPERATOR_BITWISE_OR, &t)) {
		if (
			!manage_state(
				xb_try_or     (new_state(ps.end)) &&
				emit_operation(ps.p, t.user_type)
			)
		) {
			return false;
		}
	}
	return true;
}

static bool xb_try_or(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_xor    (new_state(ps.end)) &&
			xb_try_opt_xor(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_logical_and(xcc_parser_state ps);

static bool xb_try_opt_logical_and(xcc_parser_state ps)
{
	token t;
	while (xb_match(ps.p, xbtoken::OPERATOR_LOGICAL_AND, &t)) {
		U16 jmp_addr;
		if (
			!manage_state(
				xcc_write_word    (ps.p, XWORD{XIS::DUP})       &&
				xcc_write_word    (ps.p, XWORD{XIS::PUT})       &&
				(jmp_addr = ps.p->out.size)                     &&
				xcc_write_word    (ps.p, XWORD{0})              &&
				xcc_write_word    (ps.p, XWORD{XIS::RLA})       &&
				xcc_write_word    (ps.p, XWORD{XIS::CNJMP})     &&
				xb_try_logical_and(new_state(ps.end))           &&
				emit_operation    (ps.p, t.user_type)           &&
				(ps.p->out.buffer[jmp_addr].u = ps.p->out.size)
			)
		) {
			return false;
		}
	}
	return true;
}

static bool xb_try_logical_and(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_or    (new_state(ps.end)) &&
			xb_try_opt_or(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_logical_or(xcc_parser_state ps);

static bool xb_try_opt_logical_or(xcc_parser_state ps)
{
	token t;
	while (xb_match(ps.p, xbtoken::OPERATOR_LOGICAL_OR, &t)) {
		U16 jmp_addr;
		if (
			!manage_state(
				xcc_write_word   (ps.p, XWORD{XIS::DUP})        &&
				xcc_write_word   (ps.p, XWORD{XIS::PUT})        &&
				(jmp_addr = ps.p->out.size)                     &&
				xcc_write_word   (ps.p, XWORD{0})               &&
				xcc_write_word   (ps.p, XWORD{XIS::CJMP})       &&
				xb_try_logical_or(new_state(ps.end))            &&
				emit_operation   (ps.p, t.user_type)            &&
				(ps.p->out.buffer[jmp_addr].u = ps.p->out.size)
			)
		) {
			return false;
		}
	}
	return true;
}

static bool xb_try_logical_or(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_logical_and    (new_state(ps.end)) &&
			xb_try_opt_logical_and(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_expr(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_logical_or    (new_state(ps.end)) &&
			xb_try_opt_logical_or(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_expr_list(xcc_parser_state ps, U16 *count)
{
	
	if (
		manage_state(
			(
				xb_try_expr(new_state(ps.end)) &&
				++(*count)                  &&
				(
					xb_match(ps.p, xbtoken::OPERATOR_COMMA) ?
						xb_try_expr_list(new_state(ps.end), count) :
						true
				)
			) ||
			xb_peek(ps.p).user_type == ps.end
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_new_var_list(xcc_parser_state ps);

static bool xb_try_str_lit(xcc_parser_state ps, U16 *count)
{
	token t;
	while (((t = xb_peek(ps.p)).user_type != ps.end || is_white(xb_peek(ps.p).hash)) && t.user_type != token::STOP_EOF) {
		if (!xb_try_ext_encoded_char(new_state(ps.end), t)) {
			return false;
		}
		if (!xcc_write_word(ps.p, XWORD{XIS::PUT}) || !xcc_write_word(ps.p, XWORD{U16(t.hash)})) {
			return false;
		}
		++*count;
	}
	if (!xcc_write_word(ps.p, XWORD{XIS::PUT}) || !xcc_write_word(ps.p, XWORD{U16(0)})) {
		return false;
	}
	++*count;
	return true;
}

static bool xb_try_opt_arr_def_expr(xcc_parser_state ps, xcc_symbol *sym)
{
	++sym->size;
	token p = xb_peek(ps.p);
	if (!xcc_write_rel(ps.p, sym)) {
		return false;
	}
	++ps.p->out.buffer[ps.p->out.size - 2].u;
	if (p.user_type == ps.end || p.user_type == xbtoken::OPERATOR_COMMA) {
		return
			xcc_write_word(ps.p, XWORD{XIS::PUT})           &&
			xcc_write_word(ps.p, XWORD{U16(sym->size - 1)}) &&
			xcc_write_word(ps.p, XWORD{XIS::PUSH});
	}
	U16 count = 0;
	bool is_str = false;
	if (
		manage_state(
			xb_match(ps.p, xbtoken::OPERATOR_ASSIGNMENT_SET) &&
			(
				(
					xb_match        (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_L)              &&
					xb_try_expr_list(new_state(xbtoken::OPERATOR_ENCLOSE_BRACE_R), &count) &&
					xb_match        (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)
				) ||
				(
					xb_match        (ps.p, xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE)                       &&
					(is_str = xb_try_str_lit(new_state(xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE), &count)) &&
					xb_match        (ps.p, xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE)
				)
			)
		)
	) {
		if (is_str) {
			++sym->size;
		}
		if (sym->size - 1 != count) {
			set_error(ps.p, sym->tok, xcc_error::VERIFY);
			return false;
		}
		if (count == 0) {
			set_error(ps.p, sym->tok, xcc_error::ZERO);
			return false;
		}
		return true;
	}
	return false;
}

static bool xb_try_arr_def_expr(xcc_parser_state ps, xcc_symbol *sym)
{
	token p = xb_peek(ps.p);
	if (!xcc_write_rel(ps.p, sym)) {
		return false;
	}
	++ps.p->out.buffer[ps.p->out.size - 2].u;
	U16 count = 0;
	if (
		manage_state(
			xb_match(ps.p, xbtoken::OPERATOR_ASSIGNMENT_SET) &&
			(
				(
					xb_match        (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_L)                  &&
					xb_try_expr_list(new_state(xbtoken::OPERATOR_ENCLOSE_BRACE_R), &count)     &&
					xb_match        (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)
				) ||
				(
					xb_match        (ps.p, xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE)              &&
					xb_try_str_lit  (new_state(xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE), &count) &&
					xb_match        (ps.p, xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE)
				)
			)
		)
	) {
		sym->size += count;
		if (count == 0) {
			set_error(ps.p, sym->tok, xcc_error::ZERO);
			return false;
		}
		return true;
	}
	return false;
}

static bool xb_try_new_arr_item(xcc_parser_state ps)
{
	token t;
	xcc_symbol *sym;
	if (
		manage_state(
			xb_match                       (ps.p, token::ALIAS, &t)                                    &&
			(sym = xcc_add_var(t, ps.p)) != NULL                                                       &&
			xb_match                       (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_L)                 &&
			(
				(
					xb_try_lit_expr        (new_state(xbtoken::OPERATOR_ENCLOSE_BRACKET_R), sym->size) &&
					xb_match               (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_R)                 &&
					xb_try_opt_arr_def_expr(new_state(xbtoken::OPERATOR_SEMICOLON), sym)
				)                                                                                      ||
				(
					xb_match               (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_R)                 &&
					xb_try_arr_def_expr    (new_state(xbtoken::OPERATOR_SEMICOLON), sym)
				)
			)                                                                                          &&
			(
				xb_match(ps.p, xbtoken::OPERATOR_COMMA) ?
					xb_try_new_var_list(new_state(ps.end)) :
					true
			)
		)
	) {
		if (sym->size <= 1) {
			set_error(ps.p, sym->tok, xcc_error::ZERO);
			return false;
		}
		return true;
	}
	return false;
}

static bool xb_try_opt_var_def_expr(xcc_parser_state ps)
{
	token p = xb_peek(ps.p);
	if (p.user_type == ps.end || p.user_type == xbtoken::OPERATOR_COMMA) {
		return
			xcc_write_word(ps.p, XWORD{XIS::PUT}) &&
			xcc_write_word(ps.p, XWORD{0});
	}
	if (
		manage_state(
			xb_match   (ps.p, xbtoken::OPERATOR_ASSIGNMENT_SET) &&
			xb_try_expr(new_state(xbtoken::OPERATOR_SEMICOLON))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_new_var_item(xcc_parser_state ps)
{
	token t;
	if (
		manage_state(
			xb_match               (ps.p, token::ALIAS, &t)                 &&
			xcc_add_var            (t, ps.p) != NULL                        &&
			xb_try_opt_var_def_expr(new_state(xbtoken::OPERATOR_SEMICOLON)) &&
			(
				xb_match(ps.p, xbtoken::OPERATOR_COMMA) ?
					xb_try_new_var_list(new_state(ps.end)) :
					true
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_new_var_list(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_new_arr_item(new_state(ps.end)) ||
			xb_try_new_var_item(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_new_vars(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_match           (ps.p, xbtoken::KEYWORD_TYPE_AUTO)       &&
			xb_try_new_var_list(new_state(xbtoken::OPERATOR_SEMICOLON)) &&
			xb_match           (ps.p, xbtoken::OPERATOR_SEMICOLON)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_sexpr(xcc_parser_state ps)
{
	U16 result = 0;
	if (
		manage_state(
			xb_try_lit_expr(new_state(ps.end), result) &&
			xcc_write_word(ps.p, XWORD{result})
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_sexpr_list(xcc_parser_state ps, U16 *count)
{
	if (
		manage_state(
			(
				xb_try_sexpr(new_state(ps.end)) &&
				++(*count)                      &&
				(
					xb_match(ps.p, xbtoken::OPERATOR_COMMA) ?
						xb_try_sexpr_list(new_state(ps.end), count) :
						true
				)
			) ||
			xb_peek(ps.p).user_type == ps.end
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_new_svar_list(xcc_parser_state ps);

static bool xb_try_sstr_lit(xcc_parser_state ps, U16 *count)
{
	token t;
	if (!xcc_write_word(ps.p, XWORD{XIS::PUT}) || !xcc_write_word(ps.p, XWORD{0}) || !xcc_write_word(ps.p, XWORD{XIS::RLA}) || !xcc_write_word(ps.p, XWORD{XIS::JMP})) {
		return false;
	}
	U16 jmp_addr_idx = ps.p->out.size - 3;
	while (((t = xb_peek(ps.p)).user_type != ps.end || is_white(xb_peek(ps.p).hash)) && t.user_type != token::STOP_EOF) {
		if (!xb_try_ext_encoded_char(new_state(ps.end), t)) {
			return false;
		}
		if (!xcc_write_word(ps.p, XWORD{U16(t.hash)})) {
			return false;
		}
		++*count;
	}
	if (!xcc_write_word(ps.p, XWORD{U16(0)})) {
		return false;
	}
	++*count;
	ps.p->out.buffer[jmp_addr_idx].u = ps.p->out.size;
	return true;
}

static bool xb_try_sexpr_def(xcc_parser_state ps, U16 &count)
{
	U16 jmp_addr_idx = 0;
	if (
		manage_state(
			xcc_write_word   (ps.p, XWORD{XIS::PUT})                                &&
			(jmp_addr_idx = ps.p->out.size)                                         &&
			xcc_write_word   (ps.p, XWORD{0})                                       &&
			xcc_write_word   (ps.p, XWORD{XIS::RLA})                                &&
			xcc_write_word   (ps.p, XWORD{XIS::JMP})                                &&
			xb_match         (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_L)              &&
			xb_try_sexpr_list(new_state(xbtoken::OPERATOR_ENCLOSE_BRACE_R), &count) &&
			xb_match         (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)
		)
	) {
		ps.p->out.buffer[jmp_addr_idx].u = ps.p->out.size;
		return true;
	}
	return false;
}

static bool xb_try_sarr_def_expr(xcc_parser_state ps, xcc_symbol *sym, bool verify)
{
	++sym->size;
	U16 count = 0;
	bool is_str = false;
	if (
		manage_state(
			xb_match(ps.p, xbtoken::OPERATOR_ASSIGNMENT_SET)                                             &&
			(
				xcc_write_word               (ps.p, XWORD{XIS::PUT})                                     &&
				xcc_write_word               (ps.p, XWORD{U16(ps.p->out.size + 6)})                      &&
				xcc_write_word               (ps.p, XWORD{XIS::RLA})                                     &&
				xb_try_sexpr_def             (new_state(ps.end), count)                                  ||
				(
					xb_match                 (ps.p, xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE)               &&
					(is_str = xb_try_sstr_lit(new_state(xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE), &count)) &&
					xb_match                 (ps.p, xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE)
				)
			)
		)
	) {
		if (is_str) {
			++sym->size;
		}
		if (!verify) {
			sym->size += count;
		} else if (sym->size - 1 != count) {
			set_error(ps.p, sym->tok, xcc_error::VERIFY);
			return false;
		}
		if (count == 0) {
			set_error(ps.p, sym->tok, xcc_error::ZERO);
			return false;
		}
		return true;
	}
	return false;
}

static bool xb_try_new_sarr_item(xcc_parser_state ps)
{
	token t;
	xcc_symbol *sym;
	if (
		manage_state(
			xb_match                    (ps.p, token::ALIAS, &t)                                    &&
			(sym = xcc_add_var(t, ps.p)) != NULL                                                    &&
			xb_match                    (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_L)                 &&
			(
				(
					xb_try_lit_expr     (new_state(xbtoken::OPERATOR_ENCLOSE_BRACKET_R), sym->size) &&
					xb_match            (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_R)                 &&
					xb_try_sarr_def_expr(new_state(xbtoken::OPERATOR_SEMICOLON), sym, true)
				)                                                                                   ||
				(
					xb_match            (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACKET_R)                 &&
					xb_try_sarr_def_expr(new_state(xbtoken::OPERATOR_SEMICOLON), sym, false)
				)
			)                                                                                       &&
			(
				xb_match(ps.p, xbtoken::OPERATOR_COMMA) ?
					xb_try_new_svar_list(new_state(ps.end)) :
					true
			)
		)
	) {
		sym->storage = xcc_symbol::STORAGE_STATIC;
		if (sym->size <= 1) {
			set_error(ps.p, sym->tok, xcc_error::ZERO);
			return false;
		}
		return true;
	}
	return false;
}

static bool xb_try_svar_def_expr(xcc_parser_state ps)
{
	U16 result = 0;
	if (
		manage_state(
			xb_match         (ps.p, xbtoken::OPERATOR_ASSIGNMENT_SET)         &&
			xb_try_lit_expr  (new_state(xbtoken::OPERATOR_SEMICOLON), result) &&
			xcc_write_word   (ps.p, XWORD{XIS::BIN})                          &&
			xcc_write_word   (ps.p, XWORD{result})
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_new_svar_item(xcc_parser_state ps)
{
	token t;
	if (
		manage_state(
			xb_match            (ps.p, token::ALIAS, &t)                 &&
			xcc_add_svar        (t, ps.p) != NULL                        &&
			xb_try_svar_def_expr(new_state(xbtoken::OPERATOR_SEMICOLON)) &&
			(
				xb_match(ps.p, xbtoken::OPERATOR_COMMA) ?
					xb_try_new_svar_list(new_state(ps.end)) :
					true
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_new_svar_list(xcc_parser_state ps)
{
	token t;
	if (
		manage_state(
			xb_try_new_sarr_item(new_state(ps.end)) ||
			xb_try_new_svar_item(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_new_svars(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_match            (ps.p, xbtoken::KEYWORD_TYPE_STATIC)     &&
			xb_try_new_svar_list(new_state(xbtoken::OPERATOR_SEMICOLON)) &&
			xb_match            (ps.p, xbtoken::OPERATOR_SEMICOLON)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_const_def_expr(xcc_parser_state ps, U16 &result)
{
	result = 0;
	if (
		manage_state(
			xb_match       (ps.p, xbtoken::OPERATOR_ASSIGNMENT_SET)         &&
			xb_try_lit_expr(new_state(xbtoken::OPERATOR_SEMICOLON), result)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_new_const_list(xcc_parser_state ps);

static bool xb_try_new_const_item(xcc_parser_state ps)
{
	token t;
	U16 result = 0;
	if (
		manage_state(
			xb_match             (ps.p, token::ALIAS, &t)                         &&
			xb_try_const_def_expr(new_state(xbtoken::OPERATOR_SEMICOLON), result) &&
			xcc_add_lit          (t, result, ps.p) != NULL                        &&
			(
				xb_match(ps.p, xbtoken::OPERATOR_COMMA) ?
					xb_try_new_const_list(new_state(ps.end)) :
					true
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_new_const_list(xcc_parser_state ps)
{
	token t;
	if (
		manage_state(
			xb_try_new_const_item(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_new_consts(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_match             (ps.p, xbtoken::KEYWORD_TYPE_CONST)      &&
			xb_try_new_const_list(new_state(xbtoken::OPERATOR_SEMICOLON)) &&
			xb_match             (ps.p, xbtoken::OPERATOR_SEMICOLON)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_fn_param(xcc_parser_state ps, xcc_symbol *param)
{
	token t;
	if (
		manage_state(
			xb_match(ps.p, token::ALIAS, &t)
		)
	) {
		param->param = xcc_add_param(t, ps.p);
		if (param->param == NULL) {
			return false;
		}
		return emit_empty_symbol_storage(ps.p, param->param);
	}
	return false;
}

static bool xb_try_fn_params(xcc_parser_state ps, xcc_symbol *param)
{
	if (
		manage_state(
			xb_try_fn_param(new_state(ps.end), param)                  &&
			(
				xb_peek(ps.p).user_type == ps.end                      ||
				(
					xb_match(ps.p, xbtoken::OPERATOR_COMMA)            &&
					xb_try_fn_params (new_state(ps.end), param->param)
				)
			)
		)
	) {
		return true;
	}
	return false;
}

static void set_param_addr(xcc_symbol *fn)
{
	xcc_symbol *p = fn->param;
	U16 i = 0;
	while (p != NULL) {
		p->data.u = i - fn->param_count - 1;
		p = p->param;
		++i;
	}
}

static bool xb_try_fn_noparam(xcc_parser_state ps)
{
	if (
		manage_state(
			(xb_peek(ps.p).user_type == ps.end)            ||
			(
				xb_match(ps.p, xbtoken::KEYWORD_TYPE_VOID) &&
				xb_peek(ps.p).user_type == ps.end
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_opt_fn_params(xcc_parser_state ps, bool verify_params)
{
	if (ps.p->fn == NULL) {
		set_error(ps.p, ps.p->in.last, xcc_error::INTERNAL);
		return false;
	}
	const xcc_symbol fn = *ps.p->fn;
	if (
		manage_state(
			xb_try_fn_noparam(new_state(ps.end))           ||
			xb_try_fn_params (new_state(ps.end), ps.p->fn)
		)
	) {
		ps.p->fn->param_count = 0;
		const xcc_symbol *p = ps.p->fn->param;
		while (p != NULL) {
			++ps.p->fn->param_count;
			p = p->param;
		}
		if (verify_params && ps.p->fn->param_count != fn.param_count) {
			set_error(ps.p, ps.p->fn->tok, xcc_error::VERIFY);
			return false;
		}
		set_param_addr(ps.p->fn);
		return true;
	}
	*ps.p->fn = fn;
	return false;
}

static bool xb_try_statements(xcc_parser_state ps);
static bool xb_try_statement(xcc_parser_state ps);

static bool xb_try_else(xcc_parser_state ps)
{
	U16 jmp_addr_idx = 0;
	if (
		manage_state(
			xb_match        (ps.p, xbtoken::KEYWORD_CONTROL_ELSE) &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})                 &&
			(jmp_addr_idx = ps.p->out.size)                       &&
			xcc_write_word  (ps.p, XWORD{0})                      &&
			xcc_write_word  (ps.p, XWORD{XIS::RLA})               &&
			xcc_write_word  (ps.p, XWORD{XIS::JMP})               &&
			xcc_push_scope  (ps.p, false)                         &&
			xb_try_statement(new_state(ps.end))                   &&
			emit_pop_scope  (ps.p)
		)
	) {
		ps.p->out.buffer[jmp_addr_idx].u = ps.p->out.size;
		return true;
	}
	return false;
}

static bool xb_try_if(xcc_parser_state ps)
{
	U16 jmp_addr_idx = 0;
	if (
		manage_state(
			xb_match        (ps.p, xbtoken::KEYWORD_CONTROL_IF)                  &&
			xb_match        (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)      &&
			xb_try_expr     (new_state(xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
			xb_match        (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)      &&
			xcc_write_word  (ps.p, XWORD{XIS::PUT})                              &&
			(jmp_addr_idx = ps.p->out.size)                                      &&
			xcc_write_word  (ps.p, XWORD{0})                                     &&
			xcc_write_word  (ps.p, XWORD{XIS::RLA})                              &&
			xcc_write_word  (ps.p, XWORD{XIS::CNJMP})                            &&
			xcc_push_scope  (ps.p, false)                                        &&
			xb_try_statement(new_state(ps.end))                                  &&
			emit_pop_scope  (ps.p)                                               &&
			(ps.p->out.buffer[jmp_addr_idx].u = ps.p->out.size)                  &&
			(
				!xb_try_else(new_state(ps.end)) ||
				(ps.p->out.buffer[jmp_addr_idx].u += 4)
			)

		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_while(xcc_parser_state ps)
{
	U16 jmp_addr_idx   = 0;
	U16 return_jmp_idx = ps.p->out.size;
	ps.loop_scope = ps.p->scopes.scope;
	if (
		manage_state(
			xb_match        (ps.p, xbtoken::KEYWORD_CONTROL_WHILE)               &&
			xb_match        (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)      &&
			(ps.continue_ip = ps.p->out.size)                                    &&
			xb_try_expr     (new_state(xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
			xb_match        (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)      &&
			(ps.break_ip = ps.p->out.size)                                       &&
			xcc_write_word  (ps.p, XWORD{XIS::PUT})                              &&
			(jmp_addr_idx = ps.p->out.size)                                      &&
			xcc_write_word  (ps.p, XWORD{0})                                     &&
			xcc_write_word  (ps.p, XWORD{XIS::RLA})                              &&
			xcc_write_word  (ps.p, XWORD{XIS::CNJMP})                            &&
			xcc_push_scope  (ps.p, false)                                        &&
			xb_try_statement(new_state(ps.end))                                  &&
			emit_pop_scope  (ps.p)                                               &&
			xcc_write_word  (ps.p, XWORD{XIS::PUT})                              &&
			xcc_write_word  (ps.p, XWORD{return_jmp_idx})                        &&
			xcc_write_word  (ps.p, XWORD{XIS::RLA})                              &&
			xcc_write_word  (ps.p, XWORD{XIS::JMP})                              &&
			(ps.p->out.buffer[jmp_addr_idx].u = ps.p->out.size)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_statements(xcc_parser_state ps);

static bool xb_try_scope(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_match         (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_L)      &&
			xcc_push_scope   (ps.p, false)                                  &&
			xb_try_statements(new_state(xbtoken::OPERATOR_ENCLOSE_BRACE_R)) &&
			xb_match         (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)      &&
			emit_pop_scope   (ps.p)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_expr_stmt(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_expr   (new_state(xbtoken::OPERATOR_SEMICOLON)) &&
			xb_match      (ps.p, xbtoken::OPERATOR_SEMICOLON)      &&
			xcc_write_word(ps.p, XWORD{XIS::TOSS})
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_redir_lval(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_match   (ps.p, xbtoken::OPERATOR_ARITHMETIC_MUL) &&
			xb_try_rval(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_lval(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_put_index_addr(new_state(ps.end)) ||
			xb_try_put_var_addr  (new_state(ps.end)) ||
			xb_try_redir_lval    (new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_lexpr(xcc_parser_state ps)
{
	token t;
	xcc_symbol *sym;
	if (
		manage_state(
			xb_try_lval(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_comp_ass(xcc_parser_state ps, token &op)
{
	if (
		manage_state(
			xb_match(ps.p, xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_ADD, &op) ||
			xb_match(ps.p, xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_SUB, &op) ||
			xb_match(ps.p, xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_MUL, &op) ||
			xb_match(ps.p, xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_DIV, &op) ||
			xb_match(ps.p, xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_MOD, &op) ||
			xb_match(ps.p, xbtoken::OPERATOR_BITWISE_ASSIGNMENT_AND,    &op) ||
			xb_match(ps.p, xbtoken::OPERATOR_BITWISE_ASSIGNMENT_OR,     &op) ||
			xb_match(ps.p, xbtoken::OPERATOR_BITWISE_ASSIGNMENT_XOR,    &op) ||
			xb_match(ps.p, xbtoken::OPERATOR_BITWISE_ASSIGNMENT_LSHIFT, &op) ||
			xb_match(ps.p, xbtoken::OPERATOR_BITWISE_ASSIGNMENT_RSHIFT, &op)
		)
	) {
		return true;
	}
	return false;
}

static bool emit_comp_ass(xcc_parser_state ps, const token &op)
{
	switch (op.user_type) {
	case xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_ADD: return xcc_write_word(ps.p, XWORD{XIS::ADD});
	case xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_SUB: return xcc_write_word(ps.p, XWORD{XIS::SUB});
	case xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_MUL: return xcc_write_word(ps.p, XWORD{XIS::MUL});
	case xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_DIV: return xcc_write_word(ps.p, XWORD{XIS::DIV});
	case xbtoken::OPERATOR_ARITHMETIC_ASSIGNMENT_MOD: return xcc_write_word(ps.p, XWORD{XIS::MOD});
	case xbtoken::OPERATOR_BITWISE_ASSIGNMENT_AND:    return xcc_write_word(ps.p, XWORD{XIS::AND});
	case xbtoken::OPERATOR_BITWISE_ASSIGNMENT_OR:     return xcc_write_word(ps.p, XWORD{XIS::OR});
	case xbtoken::OPERATOR_BITWISE_ASSIGNMENT_XOR:    return xcc_write_word(ps.p, XWORD{XIS::XOR});
	case xbtoken::OPERATOR_BITWISE_ASSIGNMENT_LSHIFT: return xcc_write_word(ps.p, XWORD{XIS::LSH});
	case xbtoken::OPERATOR_BITWISE_ASSIGNMENT_RSHIFT: return xcc_write_word(ps.p, XWORD{XIS::RSH});
	}
	return false;
}

static bool xb_try_comp_ass_var_stmt(xcc_parser_state ps)
{
	token op;
	if (
		manage_state(
			xb_try_lexpr   (new_state(ps.end))                      &&
			xcc_write_word (ps.p, XWORD{XIS::DUP})                  &&
			xcc_write_word (ps.p, XWORD{XIS::AT})                   &&
			xb_try_comp_ass(new_state(ps.end), op)                  &&
			xb_try_expr    (new_state(xbtoken::OPERATOR_SEMICOLON)) &&
			emit_comp_ass  (new_state(ps.end), op)                  &&
			xb_match       (ps.p, xbtoken::OPERATOR_SEMICOLON)      &&
			xcc_write_word (ps.p, XWORD{XIS::MOVD})
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_set_var_stmt(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_lexpr  (new_state(xbtoken::OPERATOR_ASSIGNMENT_SET)) &&
			xb_match      (ps.p, xbtoken::OPERATOR_ASSIGNMENT_SET)      &&
			xb_try_expr   (new_state(xbtoken::OPERATOR_SEMICOLON))      &&
			xb_match      (ps.p, xbtoken::OPERATOR_SEMICOLON)           &&
			xcc_write_word(ps.p, XWORD{XIS::MOVD})
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_reass_var_stmt(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_set_var_stmt     (new_state(ps.end)) ||
			xb_try_comp_ass_var_stmt(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_opt_expr(xcc_parser_state ps)
{
	if (
		manage_state(
			(xb_peek(ps.p).user_type == ps.end) ?
				(
					xcc_write_word(ps.p, XWORD{XIS::PUT}) &&
					xcc_write_word(ps.p, XWORD{0})
				) :
				xb_try_expr(new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_break_stmt(xcc_parser_state ps)
{
	U16 lsp = xcc_loop_stack_size(ps.p, ps.loop_scope);
	if (
		manage_state(
			xb_match      (ps.p, xbtoken::KEYWORD_CONTROL_BREAK) &&
			xb_match      (ps.p, xbtoken::OPERATOR_SEMICOLON)    &&
			(
				lsp > 0 ?
					xcc_write_word(ps.p, XWORD{XIS::PUT})        &&
					xcc_write_word(ps.p, XWORD{lsp})             &&
					xcc_write_word(ps.p, XWORD{XIS::POP}) :
					true
			)                                                    &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})                &&
			xcc_write_word(ps.p, XWORD{0})                       &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})                &&
			xcc_write_word(ps.p, XWORD{U16(ps.break_ip)})        &&
			xcc_write_word(ps.p, XWORD{XIS::RLA})                &&
			xcc_write_word(ps.p, XWORD{XIS::JMP})
		)
	) {
		if (ps.loop_scope == 0) {
			set_error(ps.p, ps.p->in.last, xcc_error::UNEXPECTED);
			return false;
		}
		return true;
	}
	return false;
}

static bool xb_try_continue_stmt(xcc_parser_state ps)
{
	U16 lsp = xcc_loop_stack_size(ps.p, ps.loop_scope);
	if (
		manage_state(
			xb_match      (ps.p, xbtoken::KEYWORD_CONTROL_CONTINUE) &&
			xb_match      (ps.p, xbtoken::OPERATOR_SEMICOLON)       &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})                   &&
			xcc_write_word(ps.p, XWORD{U16(ps.continue_ip)})        &&
			xcc_write_word(ps.p, XWORD{XIS::RLA})                   &&
			xcc_write_word(ps.p, XWORD{XIS::JMP})
		)
	) {
		if (ps.loop_scope == 0) {
			set_error(ps.p, ps.p->in.last, xcc_error::UNEXPECTED);
			return false;
		}
		return true;
	}
	return false;
}

static bool xb_try_return_stmt(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_match      (ps.p, xbtoken::KEYWORD_CONTROL_RETURN)   &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})                   &&
			xcc_write_word(ps.p, XWORD{0})                          &&
			xcc_write_word(ps.p, XWORD{XIS::RLC})                   &&
			xcc_write_word(ps.p, XWORD{XIS::AT})                    &&
			xcc_write_word(ps.p, XWORD{XIS::PUT})                   &&
			xcc_write_word(ps.p, XWORD{1})                          &&
			xcc_write_word(ps.p, XWORD{XIS::SUB})                   &&
			xb_try_opt_expr(new_state(xbtoken::OPERATOR_SEMICOLON)) &&
			xb_match       (ps.p, xbtoken::OPERATOR_SEMICOLON)      &&
			xcc_write_word(ps.p, XWORD{XIS::MOVD})                  &&
			xcc_write_word(ps.p, XWORD{XIS::LDC})                   &&
			xcc_write_word(ps.p, XWORD{XIS::JMP})
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_asm_instr(xcc_parser_state ps)
{
	if (manage_state(xasm_inline(new_state(ps.end)))) {
		return true;
	}
	return false;
}

static bool xb_try_asm_block(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_match    (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_L)                        &&
			until_end   (new_state(xbtoken::OPERATOR_ENCLOSE_BRACE_R), xb_try_asm_instr) &&
			xb_match    (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_asm_stmt(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_match(ps.p, xbtoken::KEYWORD_INTRINSIC_ASM) &&
			(
				xb_try_asm_block(new_state(ps.end)) ||
				xb_try_asm_instr(new_state(ps.end))
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_enum_val(xcc_parser_state ps, U16 &val)
{
	token t;
	U16 new_val = 0;
	bool set_val = false;
	if (
		xb_match(ps.p, token::ALIAS, &t) &&
		(
			(set_val = xb_match(ps.p, xbtoken::OPERATOR_LOGICAL_EQUAL)) ? xb_try_lit_expr(new_state(ps.end), new_val) : true
		) &&
		xcc_add_lit(t, set_val ? new_val : val, ps.p)
	) {
		if (set_val) {
			val = new_val;
		}
		return true;
	}
	return false;
}

static bool xb_try_enum_vals(xcc_parser_state ps, U16 val)
{
	if (
		manage_state(
			xb_try_enum_val(new_state(ps.end), val) &&
			(
				xb_peek(ps.p).user_type == ps.end ||
				(
					xb_match(ps.p, xbtoken::OPERATOR_COMMA) &&
					xb_try_enum_vals(new_state(ps.end), val + 1)
				)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_enum(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_match        (ps.p, xbtoken::KEYWORD_ENUM)                     &&
			xb_match        (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_L)         &&
			xb_try_enum_vals(new_state(xbtoken::OPERATOR_ENCLOSE_BRACE_R), 0) &&
			xb_match        (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)         &&
			xb_match        (ps.p, xbtoken::OPERATOR_SEMICOLON)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_statement(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_match             (ps.p, xbtoken::OPERATOR_SEMICOLON) ||
			xb_try_enum          (new_state(ps.end))                 ||
			xb_try_new_vars      (new_state(ps.end))                 ||
			xb_try_new_svars     (new_state(ps.end))                 ||
			xb_try_new_consts    (new_state(ps.end))                 ||
			xb_try_if            (new_state(ps.end))                 ||
			xb_try_while         (new_state(ps.end))                 ||
			xb_try_return_stmt   (new_state(ps.end))                 ||
			xb_try_break_stmt    (new_state(ps.end))                 ||
			xb_try_continue_stmt (new_state(ps.end))                 ||
			xb_try_scope         (new_state(ps.end))                 ||
			xb_try_reass_var_stmt(new_state(ps.end))                 ||
			xb_try_expr_stmt     (new_state(ps.end))                 ||
			xb_try_asm_stmt      (new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_statements(xcc_parser_state ps)
{
	if (
		manage_state(
			until_end(new_state(ps.end), xb_try_statement)
		)
	) {
		return true;
	}
	return false;
}

static bool adjust_fn_rel_ptr(xcc_parser_state ps)
{
	const xcc_symbol *p = ps.p->fn->param;
	U16 params = 0;
	while (p != NULL) {
		++params;
		p = p->param;
	}
	if (params == 0) {
		return true;
	}
	return
		xcc_write_word(ps.p, XWORD{XIS::PUT}) &&
		xcc_write_word(ps.p, XWORD{params})   &&
		xcc_write_word(ps.p, XWORD{XIS::SUB});
}

static bool xb_try_filepath(xcc_parser_state ps, chars::view &fp)
{
	fp = { ps.p->in.code.str + ps.p->in.head, 0, 0 };
	token t;
	while (xb_peek(ps.p).user_type != ps.end) {
		if (!xb_match1(ps.p, token::CHAR, &t)) {
			set_error(ps.p, t, xcc_error::UNEXPECTED);
			return false;
		}
		++fp.len;
	}
	return true;
}

static bool xb_try_load_text(xcc_parser_state &ps, const chars::view &source_file, xcc_text &text)
{
	if (!xcc_load_text(source_file, text)) {
		set_error(ps.p, ps.p->in.last, xcc_error::MISSING);
		return false;
	}
	
	if (xcc_add_filesum(ps.p->fsums, text.sum)) {
		ps.p->in = init_lexer(chars::view{ text.txt, text.len, 0 });
	} else {
		ps.p->in = init_lexer(chars::view{ "", 0, 0 });
	}
	ps.p->max = ps.p->in.last;
	return true;
}

static bool xb_try_global_statement(xcc_parser_state ps);
static bool xb_try_global_statements(xcc_parser_state ps);

static bool xb_try_file(xcc_parser_state ps, const chars::view &wd, const chars::view &source_file, const chars::view &ext)
{
	for (unsigned i = 0; i < wd.len; ++i) {
		ps.cwd.str[i] = wd.str[i];
	}
	ps.cwd.len = wd.len;
	if (!xcc_set_path(ps.cwd, source_file)) {
		set_error(ps.p, ps.p->in.last, xcc_error::MEMORY);
		return false;
	}
	for (unsigned i = 0; i < ext.len; ++i) {
		if (ps.cwd.len == xcc_path::MAXPATH - 1) {
			set_error(ps.p, ps.p->in.last, xcc_error::MEMORY);
			return false;
		}
		ps.cwd.str[ps.cwd.len] = ext.str[i];
		++ps.cwd.len;
	}

	xcc_text text;
	if (
		manage_state(
			xb_try_load_text        (ps, chars::view{ps.cwd.str, ps.cwd.len, 0}, text) &&
			xb_try_global_statements(new_state(token::STOP_EOF))
		)
	) {
		ps.p->fn = ps.restore_point.fn;
		ps.p->in = ps.restore_point.in;
		return true;
	}
	return false;
}

static bool xb_try_include_relative_filepath(xcc_parser_state ps)
{
	chars::view fp;
	if (
		manage_state(
			xb_match       (ps.p, xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE)          &&
			xb_try_filepath(new_state(xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE), fp) &&
			xb_match       (ps.p, xbtoken::OPERATOR_ENCLOSE_DOUBLEQUOTE)          &&
			xb_try_file    (new_state(ps.end), chars::view{ ps.cwd.str, ps.cwd.len, 0 }, fp, chars::view{ NULL, 0, 0 })
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_include_standard_filepath(xcc_parser_state ps)
{
	chars::view fp;
	if (
		manage_state(
			xb_match       (ps.p, xbtoken::OPERATOR_LOGICAL_LESS)                    &&
			xb_try_filepath(new_state(xbtoken::OPERATOR_LOGICAL_GREATER), fp)        &&
			xb_match       (ps.p, xbtoken::OPERATOR_LOGICAL_GREATER)                 &&
			xb_try_file    (new_state(ps.end), ps.swd, fp, chars::view{".xh", 3, 0}) &&
			xb_try_file    (new_state(ps.end), ps.swd, fp, chars::view{".xb", 3, 0})
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_include(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_match(ps.p, xbtoken::OPERATOR_HASH)   &&
			xb_match(ps.p, xbtoken::KEYWORD_INCLUDE) &&
			(
				xb_try_include_relative_filepath(new_state(ps.end)) ||
				xb_try_include_standard_filepath(new_state(ps.end))
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_fn_def(xcc_parser_state ps)
{
	U16   guard_jmp_idx = 0;
	token t;
	bool verify_params = false;
	if (
		manage_state(
			xb_match(ps.p, token::ALIAS, &t)                                                        &&
			(
				(verify_params = ((ps.p->fn = xcc_find_fn(t.text, ps.p)) != NULL)) ||
				(
					(ps.p->fn = xcc_add_fn(t, ps.p)) != NULL  &&
					emit_empty_symbol_storage(ps.p, ps.p->fn) &&
					(ps.p->fn->link = ps.p->out.size - 1)     &&
					xcc_write_word(ps.p, XWORD{XIS::RLA})
				)
			)                                                                                       &&
			xcc_write_word      (ps.p, XWORD{XIS::PUT})                                             &&
			(guard_jmp_idx = ps.p->out.size)                                                        &&
			xcc_write_word      (ps.p, XWORD{0})                                                    &&
			xcc_write_word      (ps.p, XWORD{XIS::RLA})                                             &&
			xcc_write_word      (ps.p, XWORD{XIS::JMP})                                             &&
			(ps.p->fn->link == 0 || (ps.p->out.buffer[ps.p->fn->link].u = ps.p->out.size))          &&
			xcc_write_word      (ps.p, XWORD{XIS::SVC})                                             &&
			xb_match            (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)                     &&
			xcc_push_scope      (ps.p)                                                              &&
			xb_try_opt_fn_params(new_state(xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R), verify_params) &&
			adjust_fn_rel_ptr   (new_state(ps.end))                                                 &&
			xcc_push_scope      (ps.p)                                                              &&
			xb_match            (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)                     &&
			xb_match            (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_L)                           &&
			xb_try_statements   (new_state(xbtoken::OPERATOR_ENCLOSE_BRACE_R))                      &&
			xb_match            (ps.p, xbtoken::OPERATOR_ENCLOSE_BRACE_R)                           &&
			emit_pop_scope      (ps.p)                                                              &&
			emit_pop_scope      (ps.p)                                                              &&
			xcc_write_word      (ps.p, XWORD{XIS::LDC})                                             &&
			xcc_write_word      (ps.p, XWORD{XIS::JMP})
		)
	) {
		ps.p->out.buffer[guard_jmp_idx].u = ps.p->out.size;
		ps.p->fn->link = 0;
		ps.p->fn = NULL;
		return true;
	}
	return false;
}

static bool xb_try_count_fn_param(xcc_parser_state ps)
{
	token t;
	if (
		manage_state(
			xb_match(ps.p, token::ALIAS, &t)
		)
	) {
		++ps.p->fn->param_count;
		return true;
	}
	return false;
}

static bool xb_try_count_fn_params(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_try_count_fn_param(new_state(ps.end)) &&
			(
				xb_peek(ps.p).user_type == ps.end ||
				(
					xb_match(ps.p, xbtoken::OPERATOR_COMMA) &&
					xb_try_count_fn_params(new_state(ps.end))
				)
			)
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_count_opt_fn_params(xcc_parser_state ps)
{
	ps.p->fn->param_count = 0;
	if (
		manage_state(
			xb_try_fn_noparam     (new_state(ps.end)) ||
			xb_try_count_fn_params(new_state(ps.end))
		)
	) {
		return true;
	}
	ps.p->fn->param_count = 0;
	return false;
}

static bool xb_try_fn_decl(xcc_parser_state ps)
{
	token t;
	if (
		manage_state(
			xb_match                    (ps.p, token::ALIAS, &t)                             &&
			(ps.p->fn = xcc_add_fn(t, ps.p)) != NULL                                         &&
			emit_empty_symbol_storage   (ps.p, ps.p->fn)                                     &&
			(ps.p->fn->link = ps.p->out.size - 1)                                            &&
			xcc_write_word              (ps.p, XWORD{XIS::RLA})                              &&
			xb_match                    (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_L)      &&
			xb_try_count_opt_fn_params  (new_state(xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)) &&
			xb_match                    (ps.p, xbtoken::OPERATOR_ENCLOSE_PARENTHESIS_R)      &&
			xb_match                    (ps.p, xbtoken::OPERATOR_SEMICOLON)
		)
	) {
		ps.p->fn = NULL;
		return true;
	}
	return false;
}

static bool xb_try_global_statement(xcc_parser_state ps)
{
	if (
		manage_state(
			xb_match         (ps.p, xbtoken::OPERATOR_SEMICOLON) ||
			xb_try_include   (new_state(ps.end))                 ||
			xb_try_enum      (new_state(ps.end))                 ||
			xb_try_fn_def    (new_state(ps.end))                 ||
			xb_try_fn_decl   (new_state(ps.end))                 ||
			xb_try_new_vars  (new_state(ps.end))                 ||
			xb_try_new_consts(new_state(ps.end))                 ||
			xb_try_new_svars (new_state(ps.end))
		)
	) {
		return true;
	}
	return false;
}

static bool xb_try_global_statements(xcc_parser_state ps)
{
	if (
		manage_state(
			until_end(new_state(ps.end), xb_try_global_statement)
		)
	) {
		return true;
	}
	return false;
}

static bool add_main(xcc_parser *p)
{
	xcc_symbol *sym = xcc_add_fn(new_token("main",4,token::ALIAS,token::ALIAS), p);
	if (sym == NULL) {
		return false;
	}
	sym->param_count = 2;
	return
		emit_empty_symbol_storage(p, sym)  &&
		(sym->link = p->out.size - 1)      &&
		xcc_write_word(p, XWORD{XIS::RLA});
}

static bool emit_call_main(xcc_parser *op)
{
	xcc_symbol *sym = xcc_find_fn(to_chars("main",4), op);
	if (sym == NULL || sym->param == NULL) {
		return true;
	}

	xcc_parser p = *op;
	p.in = init_lexer(chars::view{ "main(0,0);", 10 });
	xcc_parser_state ps = xcc_new_state(&p, NULL, token::STOP_EOF, 0, 0, 0);
	if (
		manage_state(
			xb_try_statement(new_state(ps.end))
		)
	) {
		op->out = p.out;
		return true;
	}
	return false;
}

static bool xb_try_single_program(xcc_parser_state ps)
{
	if (
		manage_state(
			xcc_write_word          (ps.p, XWORD{XIS::SVB}) &&
			add_main                (ps.p)                  &&
			xb_try_global_statements(new_state(ps.end))     &&
			emit_call_main          (ps.p)
		)
	) {
		const U16 lsp = xcc_top_scope_stack_size(ps.p);
		return
			(
				(
					lsp == 0 ||
					(
						xcc_write_word(ps.p, XWORD{XIS::PUT}) &&
						xcc_write_word(ps.p, XWORD{lsp})      &&
						xcc_write_word(ps.p, XWORD{XIS::POP})
					)
				) &&
				xcc_write_word(ps.p, XWORD{XIS::LDB})  &&
				xcc_write_word(ps.p, XWORD{XIS::HALT})
			);
	}
	return false;
}

xcc_out xb(lexer l, const chars::view &std_lib_path, xcc_binary mem, const U16 sym_capacity, const unsigned file_capacity)
{
	xcc_symbol       sym_mem[sym_capacity];
	xcc_filesum      sum_mem[file_capacity];
	xcc_parser       p  = xcc_init_parser(l, mem, sym_mem, sym_capacity, sum_mem, file_capacity);
	xcc_parser_state ps = xcc_new_state(&p, NULL, token::STOP_EOF, 0, 0, 0);
	ps.swd = std_lib_path;

	if (
		manage_state(
			xb_try_single_program(new_state(ps.end))
		)
	) {
		return xcc_out{ p.in, p.out, p.max, p.error.code != xcc_error::NONE, p.error };
	}
	set_error(ps.p, p.max, xcc_error::UNEXPECTED);
	return xcc_out{ p.in, p.out, p.max, 1, p.error };
}

static bool xb_try_files(xcc_parser_state ps, const chars::view *source_files, U16 num_source_files)
{
	for (unsigned i = 0; i < num_source_files; ++i) {
		if (
			!manage_state(
				xb_try_file(new_state(ps.end), chars::view{ NULL, 0, 0 }, source_files[i], chars::view{ NULL, 0, 0 })
			)
		) {
			return false;
		}
	}
	return true;
}

static bool xb_try_program(xcc_parser_state ps, const chars::view *source_files, U16 num_source_files)
{
	if (
		manage_state(
			xcc_write_word(ps.p, XWORD{XIS::SVB})                             &&
			add_main      (ps.p)                                              &&
			xb_try_files  (new_state(ps.end), source_files, num_source_files) &&
			emit_call_main(ps.p)
		)
	) {
		const U16 lsp = xcc_top_scope_stack_size(ps.p);
		return
			(
				(
					lsp == 0 ||
					(
						xcc_write_word(ps.p, XWORD{XIS::PUT}) &&
						xcc_write_word(ps.p, XWORD{lsp})      &&
						xcc_write_word(ps.p, XWORD{XIS::POP})
					)
				) &&
				xcc_write_word(ps.p, XWORD{XIS::LDB})  &&
				xcc_write_word(ps.p, XWORD{XIS::HALT})
			);
	}
	return false;
}

xcc_out xb(const chars::view *source_files, U16 num_source_files, const chars::view &std_lib_path, xcc_binary mem, const U16 sym_capacity, const unsigned file_capacity)
{
	xcc_symbol       sym_mem[sym_capacity];
	xcc_filesum      sum_mem[file_capacity];
	xcc_parser       p  = xcc_init_parser(init_lexer(chars::view{NULL,0,0}), mem, sym_mem, sym_capacity, sum_mem, file_capacity);
	xcc_parser_state ps = xcc_new_state(&p, NULL, token::STOP_EOF, 0, 0, 0);
	ps.swd = std_lib_path;

	if (
		manage_state(
			xb_try_program(new_state(ps.end), source_files, num_source_files)
		)
	) {
		return xcc_out{ p.in, p.out, p.max, p.error.code != xcc_error::NONE, p.error };
	}
	set_error(ps.p, p.max, xcc_error::UNEXPECTED);
	return xcc_out{ p.in, p.out, p.max, 1, p.error };
}

#undef new_state
#undef manage_state
#undef set_error

static std::string error(U16 code)
{
	switch (code) {
	case xcc_error::NONE:       return "NONE";
	case xcc_error::MEMORY:     return "MEMORY";
	case xcc_error::UNDEF:      return "UNDEF";
	case xcc_error::REDEF:      return "REDEF";
	case xcc_error::VERIFY:     return "VERIFY";
	case xcc_error::INTERNAL:   return "INTERNAL";
	case xcc_error::UNEXPECTED: return "UNEXPECTED";
	case xcc_error::MISSING:    return "MISSING";
	case xcc_error::ZERO:       return "ZERO";
	}
	return "???";
}

static bool compile(const std::string &filename, xcc_out &out)
{
	XWORD output_buffer[65536];
	std::cout << "b: compiling...";
	const chars::view files[] = {
		chars::view{ filename.c_str(), unsigned(filename.size()), 0 }
	};
	out = xb(files, 1, chars::view{"libb/", 23, 0}, xcc_binary{output_buffer, sizeof(output_buffer) / sizeof(XWORD), 0});
	std::cout << out.binary.size << " instructions" << std::endl;
	if (out.errors != 0) {
		std::cout << std::endl << "  error: fil=\'" << out.error.file.str << "\',tok=" << out.error.tok.index+1 << ",txt=\'" << out.error.tok.text.str << "\' (\"" << out.error.file.str << "\")" << " @ row=" << out.error.tok.row+1 << ",col=" << out.error.tok.col+1 << ",typ=" << error(out.error.code) << ",loc=" << out.error.ifile << "@" << out.error.iline;
		return false;
	}
	std::cout << "done" << std::endl;
	return true;
}

CMD_BEGIN(__compile)
{
	xcc_out out;
	if (!compile(std::string(params[0]), out)) {
		return 1;
	}

	std::ofstream fout(params[1], std::ios::binary);
	if (!fout.is_open()) {
		return false;
	}
	if (!fout.write((const char*)out.binary.buffer, out.binary.size * sizeof(XWORD))) {
		return false;
	}

	return true;
}
CMD_END(__compile, 2, "Compile the input source .b file and output to specified file.", false);

class Device
{
public:
	struct Packet
	{
		enum {
			TYPE_ERR,
			TYPE_CONNECT,
			TYPE_DISCONNECT,
			TYPE_PING,
			TYPE_PONG,
			TYPE_DATA,
			TYPE_MULTIPACK,
			TYPE_COUNT
		};
		enum {
			HEADER_ID,
			HEADER_CLOCK,
			HEADER_TYPE,
			HEADER_SEQ,
			HEADER_SIZE,
			HEADER_IRQ,
			HEADER_MID,
			HEADER_WORD_SIZE
		};
		static constexpr uint32_t PACKET_WORD_SIZE  = 32;
		static constexpr uint32_t PAYLOAD_WORD_SIZE = PACKET_WORD_SIZE - HEADER_WORD_SIZE;
		
		U16 header[HEADER_WORD_SIZE];
		U16 payload[PAYLOAD_WORD_SIZE];
	};

protected:
	class MessageQueue
	{
	private:
		static constexpr uint32_t CAPACITY = 256;
		Packet                    m_queue[CAPACITY];
		uint32_t                  m_start;
		uint32_t                  m_end;
	
	public:
		MessageQueue( void );
		void Pass(const Packet &msg);
		void Ack( void );
		Packet Peek( void ) const;
		uint32_t GetSize( void ) const;
		bool IsFull( void ) const;
		bool IsEmpty( void ) const;
		void Flush( void );
	};

	void Info(const char *msg) const;
	void Warn(const char *msg) const;
	void Error(const char *msg) const;

private:
	Device       *m_connection;
	MessageQueue  m_in_queue;
	std::string   m_name;
	U16           m_HWID;
	U16           m_DID;
	uint64_t      m_clock_ns;
	uint64_t      m_exec_ns;
	uint64_t      m_ns_per_cycle;
	uint32_t      m_cycles_per_second;
	uint32_t      m_external_state;
	uint32_t      m_external_state_reset[32];
	U16           m_message_id_counter;
	bool          m_power;

private:
	void CountDownExternalState(uint32_t ms);
	void ClearExternalState( void );

protected:
	static constexpr uint32_t STATE_TIMER_FOREVER = uint32_t(-1);

	void Ack( void );
	Packet Peek( void ) const;
	bool Pending( void ) const;
	void Output(const Packet &msg);
	Packet NewPacket(U16 type);
	bool Poll( void );
	void SetExternalState(uint32_t bit, bool state, uint32_t timer_ms = STATE_TIMER_FOREVER);

protected:
	virtual bool HandlePacket(const Packet &msg);
	virtual void DoCycle( void );
	virtual void DoPowerOn( void );
	virtual void DoPowerOff( void );

public:
	Device(const std::string &name, U16 HWID);
	~Device( void );
	void PowerOn( void );
	void Cycle( void );
	void Run(uint32_t ms);
	void PowerOff( void );
	void PowerToggle( void );
	void PowerCycle( void );
	bool IsPoweredOn( void ) const;
	bool IsPoweredOff( void ) const;
	void SetCyclesPerSecond(uint32_t hz);
	uint32_t GetCyclesPerSecond( void ) const;
	uint64_t GetLocalClock( void ) const;
	Device *GetConnectedDevice( void );
	const Device *GetConnectedDevice( void ) const;
	U16 GetHWID( void ) const;
	U16 GetDID( void ) const;
	std::string GetName( void ) const;
	U16 GetClock( void ) const;
	uint64_t GetHighPrecisionClock( void ) const;
	bool IsConnected(const Device &device) const;
	bool IsConnected( void ) const;
	void Disconnect( void );
	void Input(const Packet &msg);
	bool IsFull( void ) const;
	bool IsEmpty( void ) const;
	uint32_t GetExternalState( void ) const;
	static void Connect(Device &a, Device &b);
};

Device::MessageQueue::MessageQueue( void ) : m_queue(), m_start(0), m_end(0)
{}

void Device::MessageQueue::Pass(const Device::Packet &msg)
{
	if (!IsFull()) {
		m_queue[m_end] = msg;
		m_end = (m_end + 1) % CAPACITY;
	}
}

void Device::CountDownExternalState(uint32_t ms)
{
	for (uint32_t i = 0; i < 32; ++i) {
		if (m_external_state_reset[i] > 0 && m_external_state_reset[i] < STATE_TIMER_FOREVER) {
			if (ms >= m_external_state_reset[i]) {
				SetExternalState(i, false, 0);
			} else {
				m_external_state_reset[i] -= ms;
			}
		}
	}
}

void Device::ClearExternalState( void )
{
	m_external_state = 0;
	for (uint32_t i = 0; i < 32; ++i) {
		m_external_state_reset[i] = 0;
	}
}

void Device::MessageQueue::Ack( void )
{
	if (m_end != m_start) {
		m_start = (m_start + 1) % CAPACITY;
	}
}

Device::Packet Device::MessageQueue::Peek( void ) const
{
	return m_queue[m_start];
}

uint32_t Device::MessageQueue::GetSize( void ) const
{
	if (m_end < m_start) {
		return CAPACITY - m_start + m_end;
	}
	return m_end - m_start;
}

bool Device::MessageQueue::IsFull( void ) const
{
	return GetSize() == CAPACITY;
}

bool Device::MessageQueue::IsEmpty( void ) const
{
	return GetSize() == 0;
}

void Device::MessageQueue::Flush( void )
{
	m_start = 0;
	m_end = 0;
}

void Device::Info(const char *msg) const
{
	std::cout << GetClock() << " INFO  " << GetHWID() << ": " << msg << std::endl;
}

void Device::Warn(const char *msg) const
{
	std::cout << GetClock() << " WARN  " << GetHWID() << ": " << msg << std::endl;
}

void Device::Error(const char *msg) const
{
	std::cout << GetClock() << " ERROR " << GetHWID() << ": " << msg << std::endl;
}

void Device::Ack( void )
{
	return m_in_queue.Ack();
}

Device::Packet Device::Peek( void ) const
{
	return m_in_queue.Peek();
}

bool Device::Pending( void ) const
{
	return m_in_queue.GetSize() > 0;
}

void Device::Output(const Device::Packet &msg)
{
	if (m_connection != nullptr) {
		m_connection->Input(msg);
	}
}

Device::Packet Device::NewPacket(U16 type)
{
	Device::Packet p = {
		{
			GetHWID(),
			GetClock(),
			type,
			0,
			0,
			0,
			m_message_id_counter++
		}
	};
	for (uint32_t i = 0; i < sizeof(p.payload) / sizeof(U16); ++i) {
		p.payload[i] = 0;
	}
	return p;
}

bool Device::Poll( void )
{
	if (Pending()) {
		Packet p = Peek();
		Ack();
		if (p.header[Packet::HEADER_TYPE] == Packet::TYPE_PING) {
			Output(NewPacket(Packet::TYPE_PONG));
		}
		if (p.header[Packet::HEADER_TYPE] == Packet::TYPE_MULTIPACK) {
			bool ret = true;
			U16 i = 0;
			const U16 MAX_PAYLOAD = p.header[Packet::HEADER_SIZE] <= Packet::PAYLOAD_WORD_SIZE ? p.header[Packet::HEADER_SIZE] : Packet::PAYLOAD_WORD_SIZE;
			while (i < MAX_PAYLOAD) {
				Packet n;
				for (U16 j = 0; j < Packet::HEADER_WORD_SIZE; ++j) {
					n.header[j] = p.header[j];
				}
				n.header[Packet::HEADER_TYPE] = p.payload[i++];
				n.header[Packet::HEADER_SIZE] = p.payload[i++];
				n.header[Packet::HEADER_SIZE] = n.header[Packet::HEADER_SIZE] < MAX_PAYLOAD - i ? n.header[Packet::HEADER_SIZE] : MAX_PAYLOAD - i;
				for (U16 j = 0; j < n.header[Packet::HEADER_SIZE]; ++j) {
					n.payload[j] = p.payload[i++];
				}
				ret = ret & HandlePacket(n);
			}
			return ret;
		} else {
			return HandlePacket(p);
		}
	}
	return false;
}

bool Device::HandlePacket(const Packet &msg) 
{
	switch (msg.header[Packet::HEADER_TYPE]) {
		case Packet::TYPE_ERR:        /*Info("Got ERR");*/        return true;
		case Packet::TYPE_CONNECT:    /*Info("Got CONNECT");*/    return true;
		case Packet::TYPE_DISCONNECT: /*Info("Got DISCONNECT");*/ return true;
		case Packet::TYPE_PING:       /*Info("Got PING");*/       return true;
		case Packet::TYPE_PONG:       /*Info("Got PONG");*/       return true;
		case Packet::TYPE_DATA:       /*Info("Got DATA");*/       return true;
		case Packet::TYPE_MULTIPACK:  /*Info("Got MULTIPACK");*/  return true;
	}
	return false;
}

void Device::DoCycle( void )
{}

void Device::DoPowerOn( void )
{}

void Device::DoPowerOff( void )
{}

void Device::SetExternalState(uint32_t bit, bool state, uint32_t timer_ms)
{
	if (IsPoweredOn()) {
		bit = bit % 32;
		if (state) {
			m_external_state = m_external_state | (uint32_t(1) << bit);
		} else {
			m_external_state = m_external_state & ~(uint32_t(1) << bit);
		}
		m_external_state_reset[bit] = timer_ms;
	}
}

static U16 GrayCode( void )
{
	static U16 current = 0;
	U16 gray = current ^ (current << 3);
	++current;
	return gray;
}

Device::Device(const std::string &name, U16 HWID) : m_connection(nullptr), m_in_queue(), m_name(name), m_HWID(HWID), m_DID(GrayCode()), m_clock_ns(0), m_exec_ns(0), m_external_state(0), m_message_id_counter(0), m_power(false)
{
	ClearExternalState();
	SetCyclesPerSecond(60);
}

Device::~Device( void )
{
	Disconnect();
}

void Device::PowerOn( void )
{
	if (IsPoweredOff()) {
		m_power = true;
		m_clock_ns = 0;
		m_exec_ns = 0;
		m_message_id_counter = 0;
		m_in_queue.Flush();
		m_external_state = 0;
		ClearExternalState();
		Output(NewPacket(Device::Packet::TYPE_CONNECT));
		DoPowerOn();
	}
}

void Device::Cycle( void )
{
	if (m_cycles_per_second > 0 && IsPoweredOn()) {
		Poll();
		DoCycle();
		m_clock_ns += m_ns_per_cycle;
		CountDownExternalState(m_ns_per_cycle / 1000000ULL);
	}
}

void Device::Run(uint32_t ms)
{
	if (m_cycles_per_second > 0 && IsPoweredOn()) {
		m_exec_ns += uint64_t(ms) * 1000000ULL;
		while (m_cycles_per_second > 0 && m_exec_ns >= m_ns_per_cycle && IsPoweredOn()) {
			Poll();
			DoCycle();
			m_clock_ns += m_ns_per_cycle;
			m_exec_ns -= m_ns_per_cycle;
		}
		if (m_cycles_per_second == 0) {
			m_exec_ns = 0;
		}
	}
	CountDownExternalState(ms);
}

void Device::PowerOff( void )
{
	if (IsPoweredOn()) {
		DoPowerOff();
		Output(NewPacket(Device::Packet::TYPE_DISCONNECT));
		m_clock_ns = 0;
		m_exec_ns = 0;
		m_message_id_counter = 0;
		m_in_queue.Flush();
		m_external_state = 0;
		m_power = false;
		ClearExternalState();
	}
}

void Device::PowerToggle( void )
{
	if (IsPoweredOff()) {
		PowerOn();
	} else {
		PowerOff();
	}
}

void Device::PowerCycle( void )
{
	if (IsPoweredOn()) {
		PowerOff();
		PowerOn();
	}
}

bool Device::IsPoweredOn( void ) const
{
	return m_power;
}

bool Device::IsPoweredOff( void ) const
{
	return !m_power;
}

void Device::SetCyclesPerSecond(uint32_t hz)
{
	static constexpr uint64_t NS_PER_S = 1000000000ULL;
	hz = (uint64_t(hz) < NS_PER_S) ? hz : NS_PER_S;
	m_cycles_per_second = hz;
	m_ns_per_cycle = hz > 0 ? (((NS_PER_S * 10ULL) / uint64_t(hz)) + 5ULL) / 10ULL : 0ULL;
}

uint32_t Device::GetCyclesPerSecond( void ) const
{
	return m_cycles_per_second;
}

uint64_t Device::GetLocalClock( void ) const
{
	return m_clock_ns;
}

Device *Device::GetConnectedDevice( void )
{
	return m_connection;
}

const Device *Device::GetConnectedDevice( void ) const
{
	return m_connection;
}

U16 Device::GetHWID( void ) const
{
	return m_HWID;
}

U16 Device::GetDID( void ) const
{
	return m_DID;
}

std::string Device::GetName( void ) const
{
	return m_name;
}

U16 Device::GetClock( void ) const
{
	return U16(m_clock_ns / 1000000ULL);
}

uint64_t Device::GetHighPrecisionClock( void ) const
{
	return m_clock_ns;
}

bool Device::IsConnected(const Device &device) const
{
	return m_connection != &device;
}

bool Device::IsConnected( void ) const
{
	return m_connection != nullptr;
}

void Device::Disconnect( void )
{
	Output(NewPacket(Device::Packet::TYPE_DISCONNECT));
	Device *dev = m_connection;
	m_connection = nullptr;
	if (dev != nullptr) {
		dev->Disconnect();
	}
}

void Device::Input(const Device::Packet &msg)
{
	if (IsPoweredOn()) {
		if (IsFull()) { Warn("Input buffer full"); }
		m_in_queue.Pass(msg);
		if (m_cycles_per_second == 0) {
			m_clock_ns = msg.header[Packet::HEADER_CLOCK] * 1000000ULL;
			Poll();
		}
	}
}

bool Device::IsFull( void ) const
{
	return m_in_queue.IsFull();
}

bool Device::IsEmpty( void ) const
{
	return m_in_queue.IsEmpty();
}

uint32_t Device::GetExternalState( void ) const
{
	return IsPoweredOn() ? m_external_state : 0;
}

void Device::Connect(Device &a, Device &b)
{
	a.Disconnect();
	b.Disconnect();
	a.m_connection = &b;
	b.m_connection = &a;
	a.Output(a.NewPacket(Device::Packet::TYPE_CONNECT));
	b.Output(b.NewPacket(Device::Packet::TYPE_CONNECT));
}

static constexpr U16 XHWID_KB   = 0x0001;
static constexpr U16 XHWID_DISK = 0x0002;
static constexpr U16 XHWID_MON  = 0x0003;
static constexpr U16 XHWID_PWR  = 0x0004;
static constexpr U16 XHWID_TTY  = 0x0005;
static constexpr U16 XHWID_COMP = 0xffff;

class Monitor : public Device
{
public:
	static constexpr uint32_t WIDTH                    = 320;
	static constexpr uint32_t HEIGHT                   = 240;
	static constexpr uint32_t STRIDE                   = 3;
	static constexpr uint32_t PITCH                    = WIDTH * STRIDE;
	static constexpr uint32_t MEMORY_SIZE              = 4092;
	static constexpr U16      MSG_PIXMODE              = 0xface;
	static constexpr U16      MSG_TXTMODE              = 0xbeef;
	static constexpr U16      MSG_TXTMODE_LOADFONT     = 0xabcd;
	static constexpr U16      MSG_TXTMODE_LOADFONTMETA = 0xaffe;
	static constexpr U16      MSG_TXTMODE_SCROLL_DOWN  = 0x123a;
	static constexpr U16      MSG_TXTMODE_SCROLL_UP    = 0x321b;

	struct Color {
		U8 r;
		U8 g;
		U8 b;
		U8 a;
	};

	struct Colors
	{
		Color bg;
		Color fg;
	};

private:
	struct Palette {
		Color pal[16];
	};

private:
	U8       m_pixels[WIDTH*HEIGHT*3];
	U8       m_memory[MEMORY_SIZE];
	Palette  m_pal[2];
	uint32_t m_char_px_width;
	uint32_t m_char_px_height;
	uint32_t m_atlas_char_width_count;
	uint32_t m_atlas_char_height_count;
	uint32_t m_cell_px_width;
	uint32_t m_cell_px_height;
	uint32_t m_scroll;
	uint32_t m_cx, m_cy;
	U16      m_mode;
	U8       m_first_char;
	U8       m_last_char;
	U8       m_color_index;

private:
	void Clear( void );
	U8 *GetCharMap( void );
	U8 *GetColorMap( void );
	U8 *GetCurrentCharMapLine( void );
	U8 *GetCurrentColorMapLine( void );
	void DrawChar(char ch, char color_index, int x, int y);
	void DrawCharMap( void );

protected:
	void DoPowerOn( void ) override;
	void DoCycle( void ) override;
	void DoPowerOff( void ) override;
	void Newline( void );
	bool HandlePacket(const Packet &msg) override;

public:
	Monitor( void );
	void Plot(U16 x, U16 y, U8 color);
	U8 GetPixel(U16 x, U16 y) const;
	U8 *GetVideo( void );
	const U8 *GetVideo( void ) const;
	U8 *GetVideoScanline(U8 y);
	const U8 *GetVideoScanline(U8 y) const;
	const U8 *GetMemory( void ) const;
	uint32_t GetCharPxWidth( void ) const;
	uint32_t  GetCharPxHeight( void ) const;
	uint32_t GetAtlasCharWidthCount( void ) const;
	uint32_t GetAtlasCharHeightCount( void ) const;
	uint32_t GetCellPxWidth( void ) const;
	uint32_t GetCellPxHeight( void ) const;
	U16 GetMode( void ) const;
	U8 GetFirstFontChar( void ) const;
	U8 GetLastFontChar( void ) const;
	uint32_t GetCharMapWidth( void ) const;
	uint32_t GetCharMapHeight( void ) const;
	U8 *GetScrollCharMapLine( void );
	U8 *GetScrollColorMapLine( void );
	Colors GetColors(U8 color_index) const;
};

void Monitor::Clear( void )
{
	for (uint32_t i = 0; i < WIDTH * HEIGHT * STRIDE; ++i) {
		m_pixels[i] = 0;
	}
}

U8 *Monitor::GetCharMap( void )
{
	return m_memory + m_atlas_char_height_count * m_cell_px_height * m_atlas_char_width_count * (m_cell_px_width / 8);
}

U8 *Monitor::GetColorMap( void )
{
	return GetCharMap() + GetCharMapWidth() * GetCharMapHeight();
}

U8 *Monitor::GetCurrentCharMapLine( void )
{
	return GetCharMap() + GetCharMapWidth() * ((m_cy - m_scroll) % GetCharMapHeight());
}

U8 *Monitor::GetCurrentColorMapLine( void )
{
	return GetCurrentCharMapLine() + GetCharMapWidth() * GetCharMapHeight();
}

void Monitor::DrawChar(char ch, char color_index, int x, int y)
{
	if (GetAtlasCharWidthCount() == 0 || GetAtlasCharHeightCount() == 0) {
		return;
	}
	Monitor::Colors colors = GetColors(color_index);
	if (x >= 0 && y >= 0 && x + GetCharPxWidth() < Monitor::WIDTH && y + GetCharPxHeight() < Monitor::HEIGHT) {
		
		U8 *pixels = GetVideoScanline(y) + x * Monitor::STRIDE;

		if (ch >= GetFirstFontChar() && ch <= GetLastFontChar()) {

			uint32_t glyph_i = ch - GetFirstFontChar();
			uint32_t glyph_x = glyph_i % GetAtlasCharWidthCount();
			uint32_t glyph_y = glyph_i / GetAtlasCharWidthCount();

			uint32_t glyph_px_x = glyph_x * GetCellPxWidth();
			uint32_t glyph_px_y = glyph_y * GetCellPxHeight();

			const U8 *glyph = GetMemory() + ((glyph_px_y * GetAtlasCharWidthCount() * GetCellPxWidth()) + glyph_px_x) / CHAR_BIT;

			for (uint32_t y = 0; y < GetCharPxHeight(); ++y) {
				const U8 *glyph_scanline = glyph + (GetAtlasCharWidthCount() * GetCellPxWidth() / 8) * y;
				U8 *pixel_scanline = pixels + Monitor::PITCH * y;
				for (uint32_t x = 0; x < GetCharPxWidth(); ++x) {
					uint8_t p = *glyph_scanline & (1 << x);
					pixel_scanline[x * Monitor::STRIDE + 0] = p ? colors.bg.r : colors.fg.r;
					pixel_scanline[x * Monitor::STRIDE + 1] = p ? colors.bg.g : colors.fg.g;
					pixel_scanline[x * Monitor::STRIDE + 2] = p ? colors.bg.b : colors.fg.b;
				}
			}
		} else if (ch != ' ' && ch != '\n' && ch != 0 && ch != '\t' && ch != '\r') {
			for (int n = 0; n < GetCharPxWidth(); ++n) {
				pixels[n * Monitor::STRIDE + 0] = colors.fg.r;
				pixels[n * Monitor::STRIDE + 1] = colors.fg.g;
				pixels[n * Monitor::STRIDE + 2] = colors.fg.b;
			}
			pixels += Monitor::PITCH;
			for (int n = 0; n < GetCharPxHeight() - 2; ++n) {
				pixels[0] = colors.fg.r;
				pixels[1] = colors.fg.g;
				pixels[2] = colors.fg.b;
				for (int m = 1; m < GetCharPxWidth() - 1; ++m) {
					pixels[m * Monitor::STRIDE + 0] = colors.fg.r;
					pixels[m * Monitor::STRIDE + 1] = colors.fg.g;
					pixels[m * Monitor::STRIDE + 2] = colors.fg.b;
				}
				pixels[(GetCharPxWidth() - 1) * Monitor::STRIDE + 0] = colors.fg.r;
				pixels[(GetCharPxWidth() - 1) * Monitor::STRIDE + 1] = colors.fg.g;
				pixels[(GetCharPxWidth() - 1) * Monitor::STRIDE + 2] = colors.fg.b;
				pixels += Monitor::PITCH;
			}
			for (int n = 0; n < GetCharPxWidth(); ++n) {
				pixels[n * Monitor::STRIDE + 0] = colors.fg.r;
				pixels[n * Monitor::STRIDE + 1] = colors.fg.g;
				pixels[n * Monitor::STRIDE + 2] = colors.fg.b;
			}
		} else if (ch == ' ') {
			for (uint32_t y = 0; y < GetCharPxHeight(); ++y) {
				U8 *pixel_scanline = pixels + Monitor::PITCH * y;
				for (uint32_t x = 0; x < GetCharPxWidth(); ++x) {
					pixel_scanline[x * Monitor::STRIDE + 0] = colors.bg.r;
					pixel_scanline[x * Monitor::STRIDE + 1] = colors.bg.g;
					pixel_scanline[x * Monitor::STRIDE + 2] = colors.bg.b;
				}
			}
		}
	}
}

void Monitor::DrawCharMap( void )
{
	if (GetCharPxWidth() > 0 && GetCharPxHeight() > 0) {
		U8 *line = GetScrollCharMapLine();
		U8 *colors = GetScrollColorMapLine();

		for (uint32_t h = 0; h < GetCharMapHeight(); ++h) {
			const int y = h * GetCharPxHeight();
			for (uint32_t w = 0; w < GetCharMapWidth(); ++w) {
				const int x = w * GetCharPxWidth();
				DrawChar(line[w], colors[w], x, y);
			}
			line += GetCharMapWidth();
			colors += GetCharMapWidth();
		}
	}
}

void Monitor::DoPowerOn( void )
{
	Clear();
}

void Monitor::DoCycle( void )
{
	while (Poll()) {}
	Clear();
	switch (m_mode) {
	case MSG_TXTMODE:
		DrawCharMap();
		break;
	}
}

void Monitor::DoPowerOff( void )
{
	Clear();
}

void Monitor::Newline( void )
{
	m_cx = 0;
	++m_cy;
	if ((m_cy - m_scroll) >= GetCharMapHeight()) {
		++m_scroll;
		U8 *scroll = GetScrollCharMapLine();
		U8 *colors = GetScrollColorMapLine();
		for (uint32_t n = 0; n < GetCharMapWidth(); ++n) {
			scroll[n] = ' ';
			colors[n] = 0;
		}
	}
}

bool Monitor::HandlePacket(const Packet &msg) 
{
	switch (msg.header[Packet::HEADER_TYPE]) {
		case Packet::TYPE_ERR:        return true;
		case Packet::TYPE_CONNECT:    return true;
		case Packet::TYPE_DISCONNECT: return true;
		case Packet::TYPE_PING:       return true;
		case Packet::TYPE_PONG:       return true;
		case Packet::TYPE_DATA:
			if (m_mode == MSG_TXTMODE) {
				U8 *line = GetCurrentCharMapLine();
				U8 *colors = GetCurrentColorMapLine();
				for (uint32_t i = 0; i < msg.header[Device::Packet::HEADER_SIZE]; ++i) {
					if (msg.payload[i] == '\a') {
						Info("<BEEP>");
						SetExternalState(0, 1, 500);
					} else if (msg.payload[i] != '\n') {
						line[m_cx] = msg.payload[i] & 0x00ff;
						colors[m_cx] = (msg.payload[i] & 0xff00) >> 8;
						++m_cx;
						if (m_cx >= GetCharMapWidth()) {
							Newline();
							line = GetCurrentCharMapLine();
							colors = GetCurrentColorMapLine();
						}
					} else {
						Newline();
						line = GetCurrentCharMapLine();
						colors = GetCurrentColorMapLine();
					}
				}
			} else if (m_mode == MSG_PIXMODE) {
			} else {
				Error("Invalid video mode");
			}
			return true;
		case MSG_TXTMODE:
		case MSG_PIXMODE:
			Info("Got display mode");
			m_mode = msg.header[Packet::HEADER_TYPE];
			return true;
		case MSG_TXTMODE_LOADFONT:
			Info("Got font data");
			for (uint32_t i = 0; i < msg.header[Device::Packet::HEADER_SIZE]; ++i) {
				const uint32_t n = (msg.header[Device::Packet::HEADER_SEQ] * Device::Packet::PAYLOAD_WORD_SIZE + i) * 2;
				if (n >= MEMORY_SIZE - 1) {
					Error("Out of memory");
					break;
				}
				m_memory[n    ] = (msg.payload[i] & 0xFF00) >> 8;
				m_memory[n + 1] = msg.payload[i] & 0x00FF;
			}
			return true;
		case MSG_TXTMODE_SCROLL_DOWN:
			if ((m_cy - m_scroll) < GetCharMapHeight()) {
				++m_scroll;
				U8 *line = GetCurrentCharMapLine();
				U8 *colors = GetCurrentColorMapLine();
				for (uint32_t i = 0; i < GetCharMapWidth(); ++i) {
					line[i] = ' ';
					colors[i] = 0;
				}
			}
			return true;
		case MSG_TXTMODE_LOADFONTMETA:
			Info("Got font meta data");
			if (msg.header[Device::Packet::HEADER_SIZE] == 8) {
				m_char_px_width           = msg.payload[0];
				m_char_px_height          = msg.payload[1];
				m_atlas_char_width_count  = msg.payload[2];
				m_atlas_char_height_count = msg.payload[3];
				m_first_char              = msg.payload[4];
				m_last_char               = msg.payload[5];
				m_cell_px_width           = msg.payload[6];
				m_cell_px_height          = msg.payload[7];
				for (uint32_t i = 0; i < GetCharMapWidth() * GetCharMapHeight(); ++i) {
					GetCharMap()[i] = ' ';
					GetColorMap()[i] = 0;
				}
				m_cx = 0;
				m_cy = 0;
				m_scroll = 0;
			} else {
				Error("Payload size not 8");
			}
			return true;
	}
	return false;
}

Monitor::Monitor( void ) :
	Device("XERXES(tm) Multi-Color Display V452", XHWID_MON),
	m_char_px_width(0), m_char_px_height(0),
	m_atlas_char_width_count(0), m_atlas_char_height_count(0),
	m_cell_px_width(0), m_cell_px_height(0),
	m_scroll(0), m_cx(0), m_cy(0),
	m_mode(MSG_PIXMODE),
	m_first_char(0), m_last_char(0)
{
	m_pal[0].pal[0]  = Color{   0,   0,   0, 255 };
	m_pal[0].pal[1]  = Color{ 204,   0,   0, 255 };
	m_pal[0].pal[2]  = Color{  78, 154,   6, 255 };
	m_pal[0].pal[3]  = Color{ 196, 160,   0, 255 };
	m_pal[0].pal[4]  = Color{  54, 101, 164, 255 };
	m_pal[0].pal[5]  = Color{ 117,  80, 123, 255 };
	m_pal[0].pal[6]  = Color{   6, 152, 154, 255 };
	m_pal[0].pal[7]  = Color{ 211, 215, 207, 255 };
	m_pal[0].pal[8]  = Color{  85,  87,  83, 255 };
	m_pal[0].pal[9]  = Color{ 239,  41,  41, 255 };
	m_pal[0].pal[10] = Color{ 138, 226,  52, 255 };
	m_pal[0].pal[11] = Color{ 252, 233,  79, 255 };
	m_pal[0].pal[12] = Color{ 114, 159, 207, 255 };
	m_pal[0].pal[13] = Color{ 173, 127, 168, 255 };
	m_pal[0].pal[14] = Color{  52, 226, 226, 255 };
	m_pal[0].pal[15] = Color{ 255, 255, 255, 255 };

	for (unsigned i = 0; i < 16; ++i) {
		Color c = m_pal[0].pal[i];
		m_pal[1].pal[i] = Color{ U8(~c.r), U8(~c.g), U8(~c.b), c.a };
	}

	SetCyclesPerSecond(60);
}

void Monitor::Plot(U16 x, U16 y, U8 color)
{
	m_pixels[y * WIDTH + x] = color;
}

U8 Monitor::GetPixel(U16 x, U16 y) const
{
	return m_pixels[y * WIDTH + x];
}

U8 *Monitor::GetVideo( void )
{
	return m_pixels;
}

const U8 *Monitor::GetVideo( void ) const
{
	return m_pixels;
}

U8 *Monitor::GetVideoScanline(U8 y)
{
	return GetVideo() + (WIDTH * y * STRIDE);
}

const U8 *Monitor::GetVideoScanline(U8 y) const
{
	return GetVideo() + (WIDTH * y * STRIDE);
}

const U8 *Monitor::GetMemory( void ) const
{
	return m_memory;
}

uint32_t Monitor::GetCharPxWidth( void ) const
{
	return m_char_px_width;
}

uint32_t Monitor::GetCharPxHeight( void ) const
{
	return m_char_px_height;
}

uint32_t Monitor::GetAtlasCharWidthCount( void ) const
{
	return m_atlas_char_width_count;
}

uint32_t Monitor::GetAtlasCharHeightCount( void ) const
{
	return m_atlas_char_height_count;
}

uint32_t Monitor::GetCellPxWidth( void ) const
{
	return m_cell_px_width;
}

uint32_t Monitor::GetCellPxHeight( void ) const
{
	return m_cell_px_height;
}

U16 Monitor::GetMode( void ) const
{
	return m_mode;
}

U8 Monitor::GetFirstFontChar( void ) const
{
	return m_first_char;
}

U8 Monitor::GetLastFontChar( void ) const
{
	return m_last_char;
}

uint32_t Monitor::GetCharMapWidth( void ) const
{
	return WIDTH / m_char_px_width;
}

uint32_t Monitor::GetCharMapHeight( void ) const
{
	return HEIGHT / m_char_px_height;
}

U8 *Monitor::GetScrollCharMapLine( void )
{
	return GetCharMap() + GetCharMapWidth() * (m_scroll % GetCharMapHeight());
}

U8 *Monitor::GetScrollColorMapLine( void )
{
	return GetScrollCharMapLine() + GetCharMapWidth() * GetCharMapHeight();
}

Monitor::Colors Monitor::GetColors(U8 color_index) const
{
	return Colors{
		m_pal[0].pal[(color_index & 0xf0) >> 4],
		m_pal[1].pal[(color_index & 0x0f)]
	};
}

class Disk
{
private:
	uint8_t  *m_data;
	uint32_t  m_size;

public:
	explicit Disk(uint32_t size);
	~Disk( void );
	uint8_t Read(Addr32 i);
	void Write(Addr32 i, uint8_t data);
	uint32_t GetCapacity( void ) const;
	uint8_t *GetData( void );
	const uint8_t *GetData( void ) const;
};

class DiskReader : public Device
{
public:
	enum {
		MSG_TYPE_READ  = 0xf00d,
		MSG_TYPE_WRITE = 0xfeed,
		MSG_TYPE_INFO  = 0xbead
	};

private:
	Disk *m_attachment;

protected:
	bool HandlePacket(const Device::Packet &msg) override;

public:
	DiskReader( void );
	uint8_t Read(Addr32 i);
	void Write(Addr32 i, uint8_t data);
	bool HasAttachment( void ) const;
	bool Attach(Disk *disk);
	void Eject( void );
	uint32_t GetCapacity( void ) const;
};

Disk::Disk(uint32_t capacity) : m_data(new uint8_t[capacity]), m_size(capacity)
{}

Disk::~Disk( void )
{
	delete [] m_data;
	m_size = 0;
}

uint8_t Disk::Read(Addr32 i)
{
	return m_data[i.Flat() % m_size];
}

void Disk::Write(Addr32 i, uint8_t data)
{
	m_data[i.Flat() % m_size] = data;
}

uint32_t Disk::GetCapacity( void ) const
{
	return m_size;
}

uint8_t *Disk::GetData( void )
{
	return m_data;
}

const uint8_t *Disk::GetData( void ) const
{
	return m_data;
}

bool DiskReader::HandlePacket(const Device::Packet &msg) 
{
	switch (msg.header[Packet::HEADER_TYPE]) {
		case Packet::TYPE_ERR:        return true;
		case Packet::TYPE_CONNECT:    return true;
		case Packet::TYPE_DISCONNECT: return true;
		case Packet::TYPE_PING:       return true;
		case Packet::TYPE_PONG:       return true;
		case Packet::TYPE_DATA:       return true;
		case MSG_TYPE_READ:
			SetExternalState(0, 1, 16);
			if (msg.header[Packet::HEADER_SIZE] == 2) {
				Packet r = NewPacket(MSG_TYPE_READ);
				const Addr32 loc = { msg.payload[0], msg.payload[1] };
			} else {
				Error("Payload size not 2");
			}
			return true;
		case MSG_TYPE_WRITE:
			SetExternalState(0, 1, 16);
			if (msg.header[Packet::HEADER_SIZE] >= 2) {
			} else {
				Error("Payload size not at least 2");
			}
			return true;
		case MSG_TYPE_INFO:
			{
				SetExternalState(0, 1, 16);
				Packet p = NewPacket(MSG_TYPE_INFO);
				p.payload[0] = HasAttachment();
				p.payload[1] = (GetCapacity() & 0xff00) >> 16;
				p.payload[2] = GetCapacity() & 0x00ff;
				p.header[Packet::HEADER_SIZE] = 3;
				Output(p);
			}
			return true;
	}
	return false;
}

DiskReader::DiskReader( void ) : Device("XERXES(tm) Data Disk Reader", XHWID_DISK), m_attachment(nullptr)
{
	SetCyclesPerSecond(0);
}

uint8_t DiskReader::Read(Addr32 i)
{
	return HasAttachment() ? m_attachment->Read(i) : 0;
}

void DiskReader::Write(Addr32 i, uint8_t data)
{
	if (HasAttachment()) {
		m_attachment->Write(i, data);
	}
}

bool DiskReader::HasAttachment( void ) const
{
	return m_attachment != nullptr;
}

bool DiskReader::Attach(Disk *disk)
{
	if (HasAttachment()) {
		return false;
	}
	m_attachment = disk;
	return true;
}

void DiskReader::Eject( void )
{
	m_attachment = nullptr;
}

uint32_t DiskReader::GetCapacity( void ) const
{
	return HasAttachment() ? m_attachment->GetCapacity() : 0;
}

class PowerControlUnit : public Device
{
public:
	enum {
		TYPE_OFF = Device::Packet::TYPE_COUNT,
		TYPE_CYCLE
	};

protected:
	void DoPowerOn( void ) override;
	bool HandlePacket(const Device::Packet &msg) override;

public:
	PowerControlUnit( void );
};

void PowerControlUnit::DoPowerOn( void )
{
	SetExternalState(0, 1, Device::STATE_TIMER_FOREVER);
}

bool PowerControlUnit::HandlePacket(const Device::Packet &msg)
{
	switch (msg.header[Device::Packet::HEADER_TYPE]) {
	case TYPE_OFF:
		if (GetConnectedDevice() != NULL) {
			GetConnectedDevice()->PowerOff();
		}
		return true;
	case TYPE_CYCLE:
		if (GetConnectedDevice() != NULL) {
			GetConnectedDevice()->PowerCycle();
		}
		return true;
	}
	return Device::HandlePacket(msg);
}

PowerControlUnit::PowerControlUnit( void ) : Device("XERXES(tm) Power Control Unit", XHWID_PWR)
{
	SetCyclesPerSecond(0);
}

class Teleprinter : public Device
{
protected:
	bool HandlePacket(const Device::Packet &msg) override;

public:
	Teleprinter( void );
	void PrintChars(const U16 *msg, U16 size);
	void PrintNums(const U16 *msg, U16 size);
};

bool Teleprinter::HandlePacket(const Device::Packet &msg)
{
	switch (msg.header[Device::Packet::HEADER_TYPE]) {
	case Device::Packet::TYPE_DATA:
		PrintChars(msg.payload, msg.header[Device::Packet::HEADER_SIZE]);
		return true;
	}
	return Device::HandlePacket(msg);
}

Teleprinter::Teleprinter( void ) : Device("XERXES(tm) Integrated Teleprinter", XHWID_TTY)
{
	SetCyclesPerSecond(0);
}

void Teleprinter::PrintChars(const U16 *msg, U16 size)
{
	for (unsigned i = 0; i < size && msg[i] != 0; ++i) {
		if (msg[i] == '\a') {
			Info("<BEEP>");
			SetExternalState(0, 1, 500);
		} else {
			std::cout << (char)msg[i];
		}
	}
	std::cout << std::flush;
}

void Teleprinter::PrintNums(const U16 *msg, U16 size)
{
	for (unsigned i = 0; i < size; ++i) {
		std::cout << msg[i] << " ";
	}
	std::cout << std::flush;
}

class Computer : public Device
{
public:
	static constexpr uint32_t NUM_PORTS = 16;

private:
	class IOPort : public Device
	{
		friend class Computer;

	public:
		IOPort( void );
	};

	IOPort *GetPort(U16 index);
	const IOPort *GetPort(U16 index) const;
	void SetError(U16 code);
	void ClearError(U16 code);
	void Output(U16 port_index, U16 header_addr, U16 data_addr);
	void Ack(U16 port_index);
	void Peek(U16 port_index, U16 header_addr, U16 data_addr);
	bool IsFull(U16 port_index);
	bool IsEmpty(U16 port_index);

private:
	XWORD                     A,B, C, SP, IP, I, P, ERR;
	static constexpr uint32_t ROM_SIZE = 4096;
	static XWORD              ROM[ROM_SIZE];
	XWORD                     RAM[MEM_SIZE_MAX];
	DiskReader                m_internal_reader;
	PowerControlUnit          m_PCU;
	Teleprinter               m_tty;
	DiskReader                m_external_reader;
	Disk                      m_storage;
	IOPort                    m_ports[NUM_PORTS];
	bool                      m_debug;

protected:
	void DoPowerOff( void );
	void DoPowerOn( void );
	void DoCycle( void );

public:
	explicit Computer(bool debug = false);
	Computer(const Computer&) = default;
	Computer &operator=(const Computer&) = default;
	void BootDisk(const XWORD *bin, U16 bin_count);
	bool AttachDisk(Disk &disk);
	void EjectDisk( void );
	void Poke(U16 addr, XWORD val);
	void PokeA(U16 addr, XWORD val);
	void PokeB(U16 addr, XWORD val);
	void PokeC(U16 addr, XWORD val);
	void PokeTop(U16 addr, XWORD val);
	XWORD Peek(U16 addr) const;
	XWORD PeekA(U16 addr) const;
	XWORD PeekB(U16 addr) const;
	XWORD PeekC(U16 addr) const;
	XWORD PeekTop(U16 addr) const;
	bool IsAvailablePort(U8 port) const;
	void Connect(Device &device, U8 port);
	void Disconnect(U8 port);
	const Device *GetDeviceAtPort(U8 port) const;
	U16 GetPortIndex( void ) const;
	U16 InstructionPointer( void ) const;
	U16 Instruction( void ) const;
	U16 StackPointer( void ) const;
	U16 StackOffsetA( void ) const;
	U16 StackOffsetB( void ) const;
	U16 StackOffsetC( void ) const;
};

#define AT(x)         RAM[x.u]
#define ATN(x,n)      RAM[U16(x.u + (n))]

#define READI         RAM[IP.u++].u

#define TOP           RAM[SP.u]
#define LST           RAM[U16(SP.u - 1)]
#define RAT(n)        RAM[U16(SP.u - (n))]
#define SAT(n)        RAM[U16(SP.u + (n))]

#define POP_STACK(n)  SP.u -= (n)
#define PUSH_STACK(n) SP.u += (n)

#define COMP_NAME "Computer"

Computer::IOPort::IOPort( void ) : Device(COMP_NAME, XHWID_COMP)
{}

Computer::IOPort *Computer::GetPort(U16 index)
{
	return index < NUM_PORTS ? &m_ports[index] : NULL;
}

const Computer::IOPort *Computer::GetPort(U16 index) const
{
	return index < NUM_PORTS ? &m_ports[index] : NULL;
}

void Computer::SetError(U16 code)
{
	ERR.u |= U16(1 << code);
}

void Computer::ClearError(U16 code)
{
	ERR.u &= ~U16(1 << code);
}

void Computer::Output(U16 port_index, U16 header_addr, U16 data_addr)
{
	if (GetPort(port_index) != NULL) {
		Packet p;
		for (XWORD i = XWORD{0}; i.u < Packet::HEADER_WORD_SIZE; ++i.u) {
			p.header[i.u] = ATN(i, header_addr).u;
		}
		for (XWORD i = XWORD{0}; i.u < Packet::PAYLOAD_WORD_SIZE; ++i.u) {
			p.payload[i.u] = ATN(i, data_addr).u;
		}
		GetPort(port_index)->Output(p);
	} else {
		SetError(ERR_IO);
	}
}

void Computer::Ack(U16 port_index)
{
	if (GetPort(port_index) != NULL) {
		GetPort(port_index)->Ack();
	} else {
		SetError(ERR_IO);
	}
}

void Computer::Peek(U16 port_index, U16 header_addr, U16 data_addr)
{
	if (GetPort(port_index) != NULL) {
		Packet p = GetPort(port_index)->Peek();
		for (XWORD i = XWORD{0}; i.u < Packet::HEADER_WORD_SIZE; ++i.u) {
			ATN(i, header_addr).u = p.header[i.u];
		}
		for (XWORD i = XWORD{0}; i.u < Packet::PAYLOAD_WORD_SIZE; ++i.u) {
			ATN(i, data_addr).u = p.payload[i.u];
		}
	} else {
		SetError(ERR_IO);
	}
}

bool Computer::IsFull(U16 port_index)
{
	if (GetPort(port_index) != NULL && GetPort(port_index)->GetConnectedDevice() != NULL) {
		return GetPort(port_index)->GetConnectedDevice()->IsFull();
	} else {
		SetError(ERR_IO);
	}
	return true;
}

bool Computer::IsEmpty(U16 port_index)
{
	if (GetPort(port_index) != NULL) {
		return GetPort(port_index)->IsEmpty();
	} else {
		SetError(ERR_IO);
	}
	return true;
}


void Computer::DoPowerOff( void )
{
	A.u = B.u = C.u = SP.u = IP.u = ERR.u = 0;
	if (!m_debug) {
		I.u = 0;
		for (unsigned i = 0; i < MEM_SIZE_MAX; ++i) {
			AT(XWORD{U16(i)}).u = 0;
		}
	}
	for (uint32_t i = 0; i < 16; ++i) {
		m_ports[i].PowerOff();
	}
}

void Computer::DoPowerOn( void )
{
	for (uint32_t i = 0; i < 16; ++i) {
		m_ports[i].PowerOn();
	}
	A.u = B.u = C.u = SP.u = IP.u = I.u = ERR.u = 0;
	for (unsigned i = 0; i < MEM_SIZE_MAX; ++i) {
		AT(XWORD{U16(i)}).u = U16(rand());
	}
}

void Computer::DoCycle( void )
{
	I.u = READI;
	switch (I.u) {
	case XIS::NOP:
		break;
	case XIS::JMP:
		IP.u = TOP.u;
		POP_STACK(1);
		break;
	case XIS::SKIP:
		IP.u += TOP.u;
		POP_STACK(1);
		break;
	case XIS::AT:
		TOP = RAM[TOP.u];
		break;
	case XIS::ADD:
		LST.u += TOP.u;
		POP_STACK(1);
		break;
	case XIS::SUB:
		LST.u -= TOP.u;
		POP_STACK(1);
		break;
	case XIS::MUL:
		LST.u *= TOP.u;
		POP_STACK(1);
		break;
	case XIS::DIV:
		if (TOP.u != 0)      { LST.u /= TOP.u; }
		else if (LST.u != 0) { LST.u = U_MAX; SetError(ERR_DIV0); }
		else                 { LST.u = 0; }
		POP_STACK(1);
		break;
	case XIS::MOD:
		if (TOP.u != 0)      { LST.u %= TOP.u; }
		else if (LST.u != 0) { LST.u = U_MAX; SetError(ERR_DIV0); }
		else                 { LST.u = 0; }
		POP_STACK(1);
		break;
	case XIS::IADD:
		LST.i += TOP.i;
		POP_STACK(1);
		break;
	case XIS::ISUB:
		LST.i -= TOP.i;
		POP_STACK(1);
		break;
	case XIS::IMUL:
		LST.i *= TOP.i;
		POP_STACK(1);
		break;
	case XIS::IDIV:
		if (TOP.i != 0)     { LST.i /= TOP.i; }
		else if (LST.i < 0) { LST.i = I_MIN; SetError(ERR_DIV0); }
		else if (LST.i > 0) { LST.i = I_MAX; SetError(ERR_DIV0); }
		else                { LST.i = 0; }
		POP_STACK(1);
		break;
	case XIS::IMOD:
		if (TOP.i != 0)     { LST.i %= TOP.i; }
		else if (LST.i < 0) { LST.i = I_MIN; SetError(ERR_DIV0); }
		else if (LST.i > 0) { LST.i = I_MAX; SetError(ERR_DIV0); }
		else                { LST.i = 0; }
		POP_STACK(1);
		break;
	case XIS::INEG:
		TOP.i = -TOP.i;
		break;
	case XIS::LSH:
		LST.u <<= (TOP.u & 15);
		POP_STACK(1);
		break;
	case XIS::RSH:
		LST.u >>= (TOP.u & 15);
		POP_STACK(1);
		break;
	case XIS::AND:
		LST.u &= TOP.u;
		POP_STACK(1);
		break;
	case XIS::OR:
		LST.u |= TOP.u;
		POP_STACK(1);
		break;
	case XIS::XOR:
		LST.u ^= TOP.u;
		POP_STACK(1);
		break;
	case XIS::NOT:
		TOP.u = !TOP.u;
		break;
	case XIS::EQ:
		LST.u = LST.u == TOP.u ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::NE:
		LST.u = LST.u != TOP.u ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::LE:
		LST.u = LST.u <= TOP.u ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::GE:
		LST.u = LST.u >= TOP.u ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::LT:
		LST.u = LST.u < TOP.u ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::GT:
		LST.u = LST.u > TOP.u ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::IEQ:
		LST.u = LST.i == TOP.i ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::INE:
		LST.u = LST.i != TOP.i ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::ILE:
		LST.u = LST.i <= TOP.i ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::IGE:
		LST.u = LST.i >= TOP.i ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::ILT:
		LST.u = LST.i < TOP.i ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::IGT:
		LST.u = LST.i > TOP.i ? U16(1) : U16(0);
		POP_STACK(1);
		break;
	case XIS::PORT:
		P.u = TOP.u;
		POP_STACK(1);
		break;
	case XIS::PEND:
		PUSH_STACK(1);
		TOP.u = IsEmpty(P.u) ? 0 : 1;
		break;
	case XIS::FULL:
		PUSH_STACK(1);
		TOP.u = IsFull(P.u) ? 1 : 0;
		break;
	case XIS::ACK:
		Ack(P.u);
		break;
	case XIS::POLL: {
		U16 d = TOP.u, h = LST.u;
		POP_STACK(2);
		Peek(P.u, h, d);
		break;
	}
	case XIS::PASS: {
		U16 d = TOP.u, h = LST.u;
		POP_STACK(2);
		Output(P.u, h, d);
		break;
	}
	case XIS::CPUID:
		PUSH_STACK(1);
		TOP.u = GetHWID();
		break;
	case XIS::DID:
		PUSH_STACK(1);
		TOP.u = GetDID();
		break;
	case XIS::HALT:
		--IP.u;
		PowerOff();
		break;
	case XIS::PUSH:
		PUSH_STACK(TOP.u - 1);
		break;
	case XIS::POP:
		POP_STACK(TOP.u + 1);
		break;
	case XIS::PUT:
		PUSH_STACK(1);
		TOP.u = READI;
		break;
	case XIS::PUTS:
		PUSH_STACK(1);
		TOP.u = SP.u;
		break;
	case XIS::PUTI:
		PUSH_STACK(1);
		TOP.u = IP.u - 1;
		break;
	case XIS::CLOCK:
		PUSH_STACK(1);
		TOP.u = GetClock();
		break;
	case XIS::TNS:
		PUSH_STACK(1);
		TOP.u = U16(GetHighPrecisionClock() % 1000ULL);
		break;
	case XIS::TUS:
		PUSH_STACK(1);
		TOP.u = U16((GetHighPrecisionClock() / 1000ULL) % 1000ULL);
		break;
	case XIS::TMS:
		PUSH_STACK(1);
		TOP.u = U16((GetHighPrecisionClock() / 1000000ULL) % 1000ULL);
		break;
	case XIS::TS:
		PUSH_STACK(1);
		TOP.u = U16((GetHighPrecisionClock() / 1000000000ULL) % 60ULL);
		break;
	case XIS::TM:
		PUSH_STACK(1);
		TOP.u = U16((GetHighPrecisionClock() / 60000000000ULL) % 60ULL);
		break;
	case XIS::TH:
		PUSH_STACK(1);
		TOP.u = U16((GetHighPrecisionClock() / 3600000000000ULL) % 24ULL);
		break;
	case XIS::TD:
		PUSH_STACK(1);
		TOP.u = U16((GetHighPrecisionClock() / 86400000000000ULL) % 7ULL);
		break;
	case XIS::TW:
		PUSH_STACK(1);
		TOP.u = U16(GetHighPrecisionClock() / 604800000000000ULL);
		break;
	case XIS::BIN:
		READI;
		break;
	case XIS::TOSS:
		POP_STACK(1);
		break;
	case XIS::MOVD:
		RAM[LST.u] = TOP;
		POP_STACK(2);
		break;
	case XIS::MOVU:
		RAM[TOP.u] = LST;
		POP_STACK(2);
		break;
	case XIS::PEEK:
		RAM[TOP.u] = LST;
		POP_STACK(1);
		break;
	case XIS::CJMP:
		if (LST.u) {
			IP.u = TOP.u;
		}
		POP_STACK(2);
		break;
	case XIS::CSKIP:
		if (LST.u) {
			IP.u += TOP.u;
		}
		POP_STACK(2);
		break;
	case XIS::CNJMP:
		if (!LST.u) {
			IP.u = TOP.u;
		}
		POP_STACK(2);
		break;
	case XIS::CNSKIP:
		if (!LST.u) {
			IP.u += TOP.u;
		}
		POP_STACK(2);
		break;
	case XIS::DUP:
		PUSH_STACK(1);
		TOP.u = LST.u;
		break;
	case XIS::SVA:
		PUSH_STACK(1);
		TOP.u = A.u;
		PUSH_STACK(1);
		TOP.u = B.u;
		PUSH_STACK(1);
		TOP.u = C.u;
		PUSH_STACK(1);
		TOP.u = SP.u - 4;
		A.u = SP.u;
		B.u = SP.u;
		C.u = SP.u;
		break;
	case XIS::SVB:
		PUSH_STACK(1);
		TOP.u = B.u;
		PUSH_STACK(1);
		TOP.u = C.u;
		PUSH_STACK(1);
		TOP.u = SP.u - 3;
		B.u = SP.u;
		C.u = SP.u;
		break;
	case XIS::SVC:
		PUSH_STACK(1);
		TOP.u = C.u;
		PUSH_STACK(1);
		TOP.u = SP.u - 2;
		C.u = SP.u;
		break;
	case XIS::OFA:
		RAT(0).u = SP.u - RAT(0).u;
		RAT(1).u = C.u - RAT(1).u;
		RAT(2).u = B.u - RAT(2).u;
		RAT(3).u = A.u - RAT(3).u;
		A.u = SP.u;
		B.u = SP.u;
		C.u = SP.u;
		break;
	case XIS::OFB:
		RAT(0).u = SP.u - RAT(0).u;
		RAT(1).u = C.u - RAT(1).u;
		RAT(2).u = B.u - RAT(2).u;
		B.u = SP.u;
		C.u = SP.u;
		break;
	case XIS::OFC:
		RAT(0).u = SP.u - RAT(0).u;
		RAT(1).u = C.u - RAT(1).u;
		C.u = SP.u;
		break;
	case XIS::LDA:
		SP = ATN(A,  0);
		C  = ATN(A, -1);
		B  = ATN(A, -2);
		A  = ATN(A, -3);
		break;
	case XIS::LDB:
		SP = ATN(B,  0);
		C  = ATN(B, -1);
		B  = ATN(B, -2);
		break;
	case XIS::LDC:
		SP = ATN(C,  0);
		C  = ATN(C, -1);
		break;
	case XIS::RLA:
		TOP.u = TOP.u + A.u;
		break;
	case XIS::RLB:
		TOP.u = TOP.u + B.u;
		break;
	case XIS::RLC:
		TOP.u = TOP.u + C.u;
		break;
	case XIS::ERR:
		PUSH_STACK(1);
		TOP.u = ERR.u;
	case XIS::CERR:
		ClearError(TOP.u);
		POP_STACK(1);
	default:
		SetError(ERR_UNDEF);
		break;
	}
}

Computer::Computer(bool debug) : Device(COMP_NAME, XHWID_COMP), m_storage(1<<21), m_debug(debug)
{
	SetCyclesPerSecond(10000000U);

	Device::Connect(m_ports[0], m_PCU);
	Device::Connect(m_ports[1], m_tty);
	Device::Connect(m_ports[2], m_internal_reader);
	Device::Connect(m_ports[3], m_external_reader);

	m_PCU.PowerOn();
	m_tty.PowerOn();
	m_internal_reader.PowerOn();
	m_external_reader.PowerOn();

	m_internal_reader.Attach(&m_storage);
}

void Computer::BootDisk(const XWORD *bin, U16 bin_count)
{
	IP.u = A.u = B.u = C.u = SP.u = I.u = 0;
	for (U16 i = 0; i < bin_count; ++SP.u, ++i) {
		AT(SP) = bin[i];
	}
	if (m_debug) {
		for (unsigned i = SP.u; i < MEM_SIZE_MAX; ++i) {
			AT(XWORD{U16(i)}).u = XIS::HALT;
		}
	} else {
		for (unsigned i = SP.u; i < MEM_SIZE_MAX; ++i) {
			AT(XWORD{U16(i)}).u = U16(rand());
		}
	}
	SP.u -= 1;
}

bool Computer::AttachDisk(Disk &disk)
{
	return m_external_reader.Attach(&disk);
}

void Computer::EjectDisk( void )
{
	m_external_reader.Eject();
}

void Computer::Poke(U16 addr, XWORD val)
{
	RAM[addr] = val;
}

void Computer::PokeA(U16 addr, XWORD val)
{
	RAM[U16(addr + A.u)] = val;
}

void Computer::PokeB(U16 addr, XWORD val)
{
	RAM[U16(addr + B.u)] = val;
}

void Computer::PokeC(U16 addr, XWORD val)
{
	RAM[U16(addr + C.u)] = val;
}

void Computer::PokeTop(U16 addr, XWORD val)
{
	RAM[U16(addr + SP.u)] = val;
}

XWORD Computer::Peek(U16 addr) const
{
	return RAM[addr];
}

XWORD Computer::PeekA(U16 addr) const
{
	return RAM[U16(addr + A.u)];
}

XWORD Computer::PeekB(U16 addr) const
{
	return RAM[U16(addr + B.u)];
}

XWORD Computer::PeekC(U16 addr) const
{
	return RAM[U16(addr + C.u)];
}

XWORD Computer::PeekTop(U16 addr) const
{
	return RAM[U16(addr + SP.u)];
}

bool Computer::IsAvailablePort(U8 port) const
{
	return port < NUM_PORTS ? (m_ports[port % NUM_PORTS].GetConnectedDevice() == nullptr) : false;
}

void Computer::Connect(Device &device, U8 port)
{
	if (port < NUM_PORTS) {
		Disconnect(port);
		Device::Connect(m_ports[port % NUM_PORTS], device);
	}
}

void Computer::Disconnect(U8 port)
{
	if (port < NUM_PORTS) {
		m_ports[port % NUM_PORTS].Disconnect();
	}
}

const Device *Computer::GetDeviceAtPort(U8 port) const
{
	return port < NUM_PORTS && GetPort(port) != nullptr ? GetPort(port)->GetConnectedDevice() : nullptr;
}

U16 Computer::GetPortIndex( void ) const
{
	return P.u;
}

U16 Computer::InstructionPointer( void ) const
{
	return IP.u;
}

U16 Computer::Instruction( void ) const
{
	return I.u;
}

U16 Computer::StackPointer( void ) const
{
	return SP.u;
}

U16 Computer::StackOffsetA( void ) const
{
	return A.u;
}

U16 Computer::StackOffsetB( void ) const
{
	return B.u;
}

U16 Computer::StackOffsetC( void ) const
{
	return C.u;
}

JOBS_NEW(device_job)
{
protected:
	uint32_t ns2ms(uint64_t ns) const { return uint32_t((ns + 500000ULL) / 1000000ULL); }
	virtual void handle_event(const SDL_Event &event) {}

public:
	virtual Device &get_device( void ) = 0;
};

JOBS_DERIVE(demo_computer, device_job)
{
private:
	Computer      m_computer;
	SDL_Window   *m_gui;

private:
	struct Color
	{
		unsigned char r, g, b, a;
	};

private:
	void clear_screen( void );
	void draw_char(char ch, int x, int y, Color col);
	void draw_label(const char *str, int x, int y, Color col);
	void draw_filled_box(SDL_Rect r, Color col);
	void draw_outlined_box(SDL_Rect r, Color col);

protected:
	void on_birth( void );
	void on_tick(uint64_t duration_ns);

public:
	demo_computer( void );
	~demo_computer( void );

	Device &get_device( void );
	void    connect(Device &dev, U16 port);
	void    boot_disk(XWORD *bin, U16 bin_count);
};

#define FONT_ATLAS_CHAR_WIDTH_COUNT   1
#define FONT_ATLAS_CHAR_HEIGHT_COUNT 95
#define FONT_CHAR_PX_WIDTH            6
#define FONT_CHAR_PX_HEIGHT           8
#define FONT_CELL_PX_WIDTH            8
#define FONT_CELL_PX_HEIGHT           8
#define FONT_CHAR_ASCII_START        33
#define FONT_CHAR_ASCII_END         126

static const unsigned char FONT_ATLAS[] = {
	0xff, 0xfb, 0xfb, 0xfb, 0xff, 0xfb, 0xff, 0xff, 0xff, 0xeb, 0xeb, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xeb, 0xc1, 0xeb, 0xc1, 0xeb, 0xff, 0xff,
	0xff, 0xc3, 0xe9, 0xc1, 0xcb, 0xe1, 0xff, 0xff, 0xff, 0xdd, 0xef, 0xf7,
	0xfb, 0xdd, 0xff, 0xff, 0xff, 0xf1, 0xfb, 0xd5, 0xed, 0xd3, 0xff, 0xff,
	0xff, 0xfb, 0xfb, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xf7, 0xfb, 0xfb,
	0xfb, 0xf7, 0xff, 0xff, 0xff, 0xfb, 0xf7, 0xf7, 0xf7, 0xfb, 0xff, 0xff,
	0xff, 0xff, 0xeb, 0xf7, 0xeb, 0xff, 0xff, 0xff, 0xff, 0xff, 0xf7, 0xe3,
	0xf7, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xfb, 0xfd, 0xff,
	0xff, 0xff, 0xff, 0xe3, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xfb, 0xff, 0xff, 0xff, 0xdf, 0xef, 0xf7, 0xfb, 0xfd, 0xff, 0xff,
	0xff, 0xe3, 0xdd, 0xd5, 0xdd, 0xe3, 0xff, 0xff, 0xff, 0xf7, 0xf3, 0xf7,
	0xf7, 0xe3, 0xff, 0xff, 0xff, 0xe1, 0xdf, 0xe3, 0xfd, 0xc1, 0xff, 0xff,
	0xff, 0xe1, 0xdf, 0xe3, 0xdf, 0xe1, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xc1,
	0xdf, 0xdf, 0xff, 0xff, 0xff, 0xc1, 0xfd, 0xe1, 0xdf, 0xe1, 0xff, 0xff,
	0xff, 0xe3, 0xfd, 0xe1, 0xdd, 0xe3, 0xff, 0xff, 0xff, 0xc1, 0xdf, 0xcf,
	0xdf, 0xdf, 0xff, 0xff, 0xff, 0xe3, 0xdd, 0xe3, 0xdd, 0xe3, 0xff, 0xff,
	0xff, 0xc3, 0xdd, 0xc3, 0xdf, 0xdf, 0xff, 0xff, 0xff, 0xff, 0xfb, 0xff,
	0xfb, 0xff, 0xff, 0xff, 0xff, 0xff, 0xfb, 0xff, 0xfb, 0xfd, 0xff, 0xff,
	0xff, 0xf7, 0xfb, 0xfd, 0xfb, 0xf7, 0xff, 0xff, 0xff, 0xff, 0xe3, 0xff,
	0xe3, 0xff, 0xff, 0xff, 0xff, 0xfd, 0xfb, 0xf7, 0xfb, 0xfd, 0xff, 0xff,
	0xff, 0xe1, 0xdf, 0xe1, 0xff, 0xfd, 0xff, 0xff, 0xff, 0xe3, 0xcd, 0xd5,
	0xcd, 0xfb, 0xff, 0xff, 0xff, 0xf7, 0xeb, 0xdd, 0xc1, 0xdd, 0xff, 0xff,
	0xff, 0xf1, 0xed, 0xe1, 0xdd, 0xe1, 0xff, 0xff, 0xff, 0xc3, 0xfd, 0xfd,
	0xfd, 0xc3, 0xff, 0xff, 0xff, 0xe1, 0xdd, 0xdd, 0xdd, 0xe1, 0xff, 0xff,
	0xff, 0xc1, 0xfd, 0xe1, 0xfd, 0xc1, 0xff, 0xff, 0xff, 0xc1, 0xfd, 0xe1,
	0xfd, 0xfd, 0xff, 0xff, 0xff, 0xc3, 0xfd, 0xcd, 0xdd, 0xc3, 0xff, 0xff,
	0xff, 0xdd, 0xdd, 0xc1, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xe3, 0xf7, 0xf7,
	0xf7, 0xe3, 0xff, 0xff, 0xff, 0xc1, 0xdf, 0xdf, 0xdd, 0xe3, 0xff, 0xff,
	0xff, 0xdd, 0xdd, 0xe1, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xfd, 0xfd, 0xfd,
	0xfd, 0xc1, 0xff, 0xff, 0xff, 0xc9, 0xd5, 0xdd, 0xdd, 0xdd, 0xff, 0xff,
	0xff, 0xdd, 0xd9, 0xd5, 0xcd, 0xdd, 0xff, 0xff, 0xff, 0xe3, 0xdd, 0xdd,
	0xdd, 0xe3, 0xff, 0xff, 0xff, 0xe1, 0xdd, 0xe1, 0xfd, 0xfd, 0xff, 0xff,
	0xff, 0xe3, 0xdd, 0xdd, 0xcd, 0xc3, 0xff, 0xff, 0xff, 0xe1, 0xdd, 0xc1,
	0xed, 0xdd, 0xff, 0xff, 0xff, 0xc3, 0xfd, 0xe3, 0xdf, 0xe1, 0xff, 0xff,
	0xff, 0xc1, 0xf7, 0xf7, 0xf7, 0xf7, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd,
	0xdd, 0xe3, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xeb, 0xf7, 0xff, 0xff,
	0xff, 0xdd, 0xdd, 0xdd, 0xd5, 0xc9, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xe3,
	0xdd, 0xdd, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xeb, 0xf7, 0xf7, 0xff, 0xff,
	0xff, 0xc1, 0xdf, 0xe3, 0xfd, 0xc1, 0xff, 0xff, 0xff, 0xf3, 0xfb, 0xfb,
	0xfb, 0xf3, 0xff, 0xff, 0xff, 0xfd, 0xfb, 0xf7, 0xef, 0xdf, 0xff, 0xff,
	0xff, 0xf3, 0xf7, 0xf7, 0xf7, 0xf3, 0xff, 0xff, 0xff, 0xf7, 0xeb, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xc1, 0xff, 0xff,
	0xff, 0xfb, 0xf7, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xc3, 0xdd,
	0xdd, 0xc3, 0xff, 0xff, 0xff, 0xfd, 0xfd, 0xe1, 0xdd, 0xe1, 0xff, 0xff,
	0xff, 0xff, 0xc3, 0xfd, 0xfd, 0xc3, 0xff, 0xff, 0xff, 0xdf, 0xdf, 0xc3,
	0xdd, 0xc3, 0xff, 0xff, 0xff, 0xff, 0xe3, 0xc1, 0xfd, 0xc3, 0xff, 0xff,
	0xff, 0xe7, 0xfb, 0xf3, 0xfb, 0xfb, 0xff, 0xff, 0xff, 0xff, 0xc3, 0xdd,
	0xc3, 0xdf, 0xe3, 0xff, 0xff, 0xfd, 0xfd, 0xe1, 0xdd, 0xdd, 0xff, 0xff,
	0xff, 0xf7, 0xff, 0xf3, 0xf7, 0xe3, 0xff, 0xff, 0xff, 0xdf, 0xff, 0xdf,
	0xdf, 0xdd, 0xe3, 0xff, 0xff, 0xfd, 0xdd, 0xe1, 0xdd, 0xdd, 0xff, 0xff,
	0xff, 0xfb, 0xfb, 0xfb, 0xfb, 0xe7, 0xff, 0xff, 0xff, 0xff, 0xe1, 0xd5,
	0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0xe1, 0xdd, 0xdd, 0xdd, 0xff, 0xff,
	0xff, 0xff, 0xe3, 0xdd, 0xdd, 0xe3, 0xff, 0xff, 0xff, 0xff, 0xe1, 0xdd,
	0xe1, 0xfd, 0xfd, 0xff, 0xff, 0xff, 0xc3, 0xdd, 0xc3, 0xdf, 0xdf, 0xff,
	0xff, 0xff, 0xc5, 0xf9, 0xfd, 0xfd, 0xff, 0xff, 0xff, 0xff, 0xc3, 0xf9,
	0xc7, 0xe1, 0xff, 0xff, 0xff, 0xfb, 0xf3, 0xfb, 0xfb, 0xe7, 0xff, 0xff,
	0xff, 0xff, 0xdd, 0xdd, 0xdd, 0xe3, 0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd,
	0xeb, 0xf7, 0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd, 0xd5, 0xeb, 0xff, 0xff,
	0xff, 0xff, 0xdd, 0xe3, 0xdd, 0xdd, 0xff, 0xff, 0xff, 0xff, 0xdd, 0xdd,
	0xc3, 0xdf, 0xe3, 0xff, 0xff, 0xff, 0xc1, 0xef, 0xf3, 0xc1, 0xff, 0xff,
	0xff, 0xe7, 0xf7, 0xfb, 0xf7, 0xe7, 0xff, 0xff, 0xff, 0xfb, 0xfb, 0xfb,
	0xfb, 0xfb, 0xff, 0xff, 0xff, 0xf3, 0xf7, 0xef, 0xf7, 0xf3, 0xff, 0xff,
	0xff, 0xeb, 0xf5, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xc1, 0xc1, 0xc1,
	0xc1, 0xc1, 0xff, 0xff
};

static constexpr uint32_t WINDOW_WIDTH = 128*3;
static constexpr uint32_t WINDOW_HEIGHT = 128;

void demo_computer::clear_screen( void )
{
	SDL_Surface *surf = SDL_GetWindowSurface(m_gui);
	if (surf != NULL) {
		SDL_Rect     rect = { 0, 0, surf->w, surf->h };
		SDL_FillRect(surf, &rect, 0);
	}
}

void demo_computer::draw_char(char ch, int x, int y, Color col)
{
	if (x >= 0 && y >= 0 && x + FONT_CHAR_PX_WIDTH < WINDOW_WIDTH && y + FONT_CHAR_PX_HEIGHT < WINDOW_HEIGHT) {
		SDL_Surface *surf = SDL_GetWindowSurface(m_gui);
		U8 *pixels = (U8*)(surf->pixels) + (y * surf->pitch) + x * surf->format->BytesPerPixel;

		if (ch >= FONT_CHAR_ASCII_START && ch <= FONT_CHAR_ASCII_END) {

			uint32_t glyph_i = ch - FONT_CHAR_ASCII_START;
			uint32_t glyph_x = glyph_i % FONT_ATLAS_CHAR_WIDTH_COUNT;
			uint32_t glyph_y = glyph_i / FONT_ATLAS_CHAR_WIDTH_COUNT;

			uint32_t glyph_px_x = glyph_x * FONT_CELL_PX_WIDTH;
			uint32_t glyph_px_y = glyph_y * FONT_CELL_PX_HEIGHT;

			const U8 *glyph = FONT_ATLAS + ((glyph_px_y * FONT_ATLAS_CHAR_WIDTH_COUNT * FONT_CELL_PX_WIDTH) + glyph_px_x) / CHAR_BIT;

			for (uint32_t y = 0; y < FONT_CHAR_PX_HEIGHT; ++y) {
				const U8 *glyph_scanline = glyph + (FONT_ATLAS_CHAR_WIDTH_COUNT * FONT_CELL_PX_WIDTH / 8) * y;
				U8 *pixel_scanline = pixels + surf->pitch * y;
				for (uint32_t x = 0; x < FONT_CHAR_PX_WIDTH; ++x) {
					if (!(*glyph_scanline & (1 << x))) {
						pixel_scanline[x * surf->format->BytesPerPixel + 2] = col.r;
						pixel_scanline[x * surf->format->BytesPerPixel + 1] = col.g;
						pixel_scanline[x * surf->format->BytesPerPixel + 0] = col.b;
					}
				}
			}
		} else if (ch != ' ' && ch != '\n' && ch != 0 && ch != '\t' && ch != '\r') {
			for (int n = 0; n < FONT_CHAR_PX_WIDTH; ++n) {
				pixels[n * surf->format->BytesPerPixel + 2] = col.r;
				pixels[n * surf->format->BytesPerPixel + 1] = col.g;
				pixels[n * surf->format->BytesPerPixel + 0] = col.b;
			}
			pixels += surf->pitch;
			for (int n = 0; n < FONT_CHAR_PX_HEIGHT - 2; ++n) {
				pixels[2]                                                          = col.r;
				pixels[1]                                                          = col.g;
				pixels[0]                                                          = col.b;
				pixels[(FONT_CHAR_PX_WIDTH - 1) * surf->format->BytesPerPixel + 2] = col.r;
				pixels[(FONT_CHAR_PX_WIDTH - 1) * surf->format->BytesPerPixel + 1] = col.g;
				pixels[(FONT_CHAR_PX_WIDTH - 1) * surf->format->BytesPerPixel + 0] = col.b;
				pixels += surf->pitch;
			}
			for (int n = 0; n < FONT_CHAR_PX_WIDTH; ++n) {
				pixels[n * surf->format->BytesPerPixel + 2] = col.r;
				pixels[n * surf->format->BytesPerPixel + 1] = col.g;
				pixels[n * surf->format->BytesPerPixel + 0] = col.b;
			}
		}
	}
}

void demo_computer::draw_label(const char *str, int x, int y, Color col)
{
	SDL_Surface *surf = SDL_GetWindowSurface(m_gui);
	if (surf != NULL) {
		if (SDL_MUSTLOCK(surf)) {
			SDL_LockSurface(surf);
		}
		
		while (*str != 0) {
			draw_char(*str, x, y, col);
			x += FONT_CHAR_PX_WIDTH;
			++str;
		}

		if (SDL_MUSTLOCK(surf)) {
			SDL_UnlockSurface(surf);
		}
	}
}

void demo_computer::draw_filled_box(SDL_Rect r, Color col)
{
	SDL_Surface *surf = SDL_GetWindowSurface(m_gui);
	if (surf != NULL) {
		SDL_FillRect(surf, &r, SDL_MapRGB(surf->format, col.r, col.g, col.b));
	}
}

void demo_computer::draw_outlined_box(SDL_Rect r, Color col)
{
	SDL_Surface *surf = SDL_GetWindowSurface(m_gui);
	if (surf != NULL) {
		if (SDL_MUSTLOCK(surf)) {
			SDL_LockSurface(surf);
		}
		uint8_t *pixels = ((uint8_t*)surf->pixels) + r.y * surf->pitch + r.x * surf->format->BytesPerPixel;
		for (int32_t x = 0; x < r.w; ++x) {
			pixels[x * surf->format->BytesPerPixel + 2] = col.r;
			pixels[x * surf->format->BytesPerPixel + 1] = col.g;
			pixels[x * surf->format->BytesPerPixel + 0] = col.b;
		}
		pixels += surf->pitch;
		for (int32_t y = 1; y < r.h - 1; ++y) {
			uint8_t *pixel = pixels;
			pixel[2] = col.r;
			pixel[1] = col.g;
			pixel[0] = col.b;
			pixel += surf->format->BytesPerPixel;
			for (int32_t x = 1; x < r.w - 1; ++x) {
				for (int32_t i = 0; i < surf->format->BytesPerPixel; ++i) {
					*pixel = 0;
					++pixel;
				}
			}
			pixel[2] = col.r;
			pixel[1] = col.g;
			pixel[0] = col.b;
			pixels += surf->pitch;
		}
		for (int32_t x = 0; x < r.w; ++x) {
			pixels[x * surf->format->BytesPerPixel + 2] = col.r;
			pixels[x * surf->format->BytesPerPixel + 1] = col.g;
			pixels[x * surf->format->BytesPerPixel + 0] = col.b;
		}
		if (SDL_MUSTLOCK(surf)) {
			SDL_UnlockSurface(surf);
		}
	}
}

void demo_computer::on_birth( void )
{
	m_computer.PowerOn();
}

void demo_computer::on_tick(uint64_t duration_ns)
{
	m_computer.Run(ns2ms(duration_ns));

	clear_screen();

	SDL_Rect led = SDL_Rect{
		WINDOW_HEIGHT/ 2 - 50, WINDOW_HEIGHT / 2 - 50,
		100, 100
	};
	if (m_computer.IsPoweredOn()) {
		draw_filled_box(led, Color{0x00, 0xff, 0x00, 0xff});
		draw_label("PWR", led.x + 2, led.y + 2, Color{ 0x00, 0x00, 0x00, 0xff });
	} else {
		draw_outlined_box(led, Color{0x00, 0xff, 0x00, 0xff});
		draw_label("PWR", led.x + 2, led.y + 2, Color{ 0x00, 0xff, 0x00, 0xff });
	}
	led.x += led.w + 25;
	if (m_computer.GetDeviceAtPort(1)->GetExternalState() || m_computer.GetDeviceAtPort(15)->GetExternalState()) {
		draw_filled_box(led, Color{0xff, 0x00, 0, 0xff});
		draw_label("BELL", led.x + 2, led.y + 2, Color{ 0x00, 0x00, 0x00, 0xff });
	} else {
		draw_outlined_box(led, Color{0xff, 0x00, 0, 0xff});
		draw_label("BELL", led.x + 2, led.y + 2, Color{ 0xff, 0x00, 0x00, 0xff });
	}
	led.x += led.w + 25;
	if (m_computer.GetDeviceAtPort(2)->GetExternalState()) {
		draw_filled_box(led, Color{0xff, 0xef, 0, 0xff});
		draw_label("DISK", led.x + 2, led.y + 2, Color{ 0x00, 0x00, 0x00, 0xff });
	} else {
		draw_outlined_box(led, Color{0xff, 0xef, 0, 0xff});
		draw_label("DISK", led.x + 2, led.y + 2, Color{ 0xff, 0xef, 0x00, 0xff });
	}

	SDL_UpdateWindowSurface(m_gui);
}

demo_computer::demo_computer( void ) : m_gui(nullptr)
{
	m_gui = SDL_CreateWindow(
		m_computer.GetName().c_str(),
		SDL_WINDOWPOS_CENTERED,
		SDL_WINDOWPOS_CENTERED,
		WINDOW_WIDTH,
		WINDOW_HEIGHT,
		SDL_WINDOW_OPENGL
	);
}

demo_computer::~demo_computer( void )
{
	if (m_gui != nullptr) {
		SDL_DestroyWindow(m_gui);
		m_gui = nullptr;
	}
}

Device &demo_computer::get_device( void )
{
	return m_computer;
}

void demo_computer::connect(Device &dev, U16 port)
{
	m_computer.Connect(dev, port);
}

void demo_computer::boot_disk(XWORD *bin, U16 bin_count)
{
	m_computer.BootDisk(bin, bin_count);
}

JOBS_DERIVE(demo_monitor, device_job)
{
private:
	Monitor     m_monitor;
	SDL_Window *m_display;

private:
	void transfer_pixels(const U8 *pixels);
	void clear_screen( void );

protected:
	void on_birth( void );
	void on_tick(uint64_t duration_ns);

public:
	demo_monitor( void );
	~demo_monitor( void );

	Device &get_device( void );
};

struct RGBA
{
	U8 r, g, b, a;
};

RGBA decode(U8 pixel)
{
	constexpr unsigned SCALAR3 = (256<<8) / 0b111;
	constexpr unsigned SCALAR2 = (256<<8) /  0b11;

	U8 r = unsigned((pixel & 0b11100000) * SCALAR3) >> 13;
	U8 g = unsigned((pixel & 0b00011100) * SCALAR3) >> 10;
	U8 b = unsigned((pixel & 0b00000011) * SCALAR2) >>  8;

	return RGBA{
		r, g, b, 0xff
	};
}

void demo_monitor::transfer_pixels(const U8 *pixels)
{
	SDL_Surface *surf  = SDL_GetWindowSurface(m_display);
	U8          *video;

	if (SDL_MUSTLOCK(surf)) {
		SDL_LockSurface(surf);
	}

	for (int32_t y = 0; y < Monitor::HEIGHT; ++y) {
		video = (U8*)(surf->pixels) + surf->pitch * (y * 3);
		for (int32_t x = 0; x < Monitor::WIDTH; ++x) {
			U8 r = pixels[(x + y * Monitor::WIDTH) * Monitor::STRIDE + 0];
			U8 g = pixels[(x + y * Monitor::WIDTH) * Monitor::STRIDE + 1];
			U8 b = pixels[(x + y * Monitor::WIDTH) * Monitor::STRIDE + 2];
			video[0] = b;
			video += surf->format->BytesPerPixel;
			video[1] = g;
			video += surf->format->BytesPerPixel;
			video[2] = r;
			video += surf->format->BytesPerPixel;
		}
		video = (U8*)(surf->pixels) + surf->pitch * (y * 3 + 1);
		for (int32_t x = 0; x < surf->pitch; ++x) {
			video[x] = video[x - surf->pitch];
		}
		video = (U8*)(surf->pixels) + surf->pitch * (y * 3 + 2);
		for (int32_t x = 0; x < surf->pitch; ++x) {
			video[x] = video[x - surf->pitch] >> 1;
		}
	}

	if (SDL_MUSTLOCK(surf)) {
		SDL_UnlockSurface(surf);
	}
}

void demo_monitor::clear_screen( void )
{
	SDL_Surface *surf = SDL_GetWindowSurface(m_display);
	SDL_Rect     rect = { 0, 0, surf->w, surf->h };
	SDL_FillRect(SDL_GetWindowSurface(m_display), &rect, 0);
}

void demo_monitor::on_birth( void )
{
	m_monitor.PowerOn();
}

void demo_monitor::on_tick(uint64_t duration_ns)
{
	bool is_on = m_monitor.IsPoweredOn();
	m_monitor.Run(ns2ms(duration_ns));
	if (m_monitor.IsPoweredOn()) {
		if (m_display != nullptr) {
			transfer_pixels(m_monitor.GetVideo());
			SDL_UpdateWindowSurface(m_display);
		}
	} else if (is_on) {
		clear_screen();
		SDL_UpdateWindowSurface(m_display);
	}
}

demo_monitor::demo_monitor( void )
{
	m_display = SDL_CreateWindow(
		m_monitor.GetName().c_str(),
		SDL_WINDOWPOS_CENTERED,
		SDL_WINDOWPOS_CENTERED,
		Monitor::WIDTH * 3,
		Monitor::HEIGHT * 3,
		SDL_WINDOW_OPENGL
	);
}

demo_monitor::~demo_monitor( void )
{
	if (m_display != nullptr) {
		SDL_DestroyWindow(m_display);
		m_display = nullptr;
	}
}

Device &demo_monitor::get_device( void )
{
	return m_monitor;
}

bool demo_init( void );

void demo_close( void );

JOBS_NEW(application_root)
{
private:
	job::ref<demo_computer> m_pc;

protected:
	void on_birth( void );
	void on_tick(uint64_t duration_ns);

public:
//	~application_root();
};

bool demo_init( void )
{
	if (SDL_Init(SDL_INIT_VIDEO) < 0) {
		return false;
	}
	atexit(demo_close);
	return true;
}

void demo_close( void )
{
	SDL_Quit();
}

void application_root::on_birth( void )
{
	job::query::results results = get_children<demo_computer>();
	job::query::result *result = results.get_results();
	demo_computer *pc;
	if (result != nullptr && (pc = result->get_job()->cast<demo_computer>()) != nullptr) {
		m_pc = pc;
	}
}

void application_root::on_tick(uint64_t duration_ns)
{
	SDL_Event event;
	while (SDL_PollEvent(&event)) {
		std::cout << event.type << std::endl;
		switch (event.type) {
		case SDL_WINDOWEVENT_CLOSE:
		case SDL_QUIT:
			kill();
			break;
		}
	}

	if (!is_killed()) {
		job::query::results results = get_children<demo_computer>();
		job::query::result *result = results.get_results();
		demo_computer *pc;
		if (result != nullptr && (pc = result->get_job()->cast<demo_computer>()) != nullptr && pc->get_device().IsPoweredOff()) {
			kill();
		}
	}
}

CMD_BEGIN(__run)
{
	if (!demo_init()) {
		std::cout << "init failed" << std::endl;
		return false;
	}

	application_root root;
	root.limit_tick_interval(16666667ULL, 16666667ULL);
	demo_computer *pc = root.add_child<demo_computer>();
	pc->connect(root.add_child<demo_monitor>()->get_device(), 15);

	{
		const std::string BIN = buffer_file(std::string(params[0]));
		std::cout << "instructions=" << BIN.size() / sizeof(XWORD) << std::endl;
		pc->boot_disk((XWORD*)(BIN.c_str()), unsigned(BIN.size()) / sizeof(XWORD));
	}
	root.run();

	return true;
}
CMD_END(__run, 1, "Demoes a ROM program by running it in a visual environment.", false);

class xdebugger
{
public:
	Computer m_computer;
	Monitor  m_monitor;

public:
	xdebugger(const xcc_binary &program);
	bool step( void );
	void ui(unsigned rows = 25) const;
	static chars decode(U16 instruction);
};

void print_padded_hex(U16 num)
{
	for (signed i = 3; i >= 0; --i) {
		unsigned x = (num & (0xf << (i*4))) >> (i*4);
		switch (x) {
		case 0:
		case 1:
		case 2:
		case 3:
		case 4:
		case 5:
		case 6:
		case 7:
		case 8:
		case 9:
			std::cout << x;
			break;
		case 10:
			std::cout << 'a';
			break;
		case 11:
			std::cout << 'b';
			break;
		case 12:
			std::cout << 'c';
			break;
		case 13:
			std::cout << 'd';
			break;
		case 14:
			std::cout << 'e';
			break;
		case 15:
			std::cout << 'f';
			break;
		}
	}
}

xdebugger::xdebugger(const xcc_binary &program) : m_computer(true)
{
	m_computer.PowerOn();
	m_computer.BootDisk(program.buffer, program.size);

	m_monitor.PowerOn();
	m_computer.Connect(m_monitor, 15);
}

bool xdebugger::step( void )
{
	m_computer.Cycle();
	return m_computer.IsPoweredOn();
}

void xdebugger::ui(unsigned rows) const
{
	const int i_page_width  = 4;
	const int i_page_height = rows;
	const int i_page_size   = i_page_width * i_page_height;
	
	const int start_i = (m_computer.InstructionPointer() / i_page_size) * i_page_size;
	
	int stack_size = signed(m_computer.StackPointer()) - m_computer.StackOffsetC();
	if (stack_size < 0) { stack_size = 0; }

	std::cout << "╔════════════════════════════════════════════════════════════════════════╗" << std::endl;
	std::cout << "║                                        A=";
	print_padded_hex(m_computer.StackOffsetA());
	std::cout << "         NS=";
	print_padded_hex(m_computer.GetHighPrecisionClock() % 1000ULL);
	std::cout << "          ║" << std::endl;
	std::cout << "║                                        B=";
	print_padded_hex(m_computer.StackOffsetB());
	std::cout << "         US=";
	print_padded_hex((m_computer.GetHighPrecisionClock() / 1000ULL) % 1000ULL);
	std::cout << "          ║" << std::endl;
	std::cout << "║                                        C=";
	print_padded_hex(m_computer.StackOffsetC());
	std::cout << "         MS=";
	print_padded_hex((m_computer.GetHighPrecisionClock() / 1000000ULL) % 1000ULL);
	std::cout << "          ║" << std::endl;
	std::cout << "║ LIST   PROG                    INST    S=";
	print_padded_hex(m_computer.StackPointer());
	std::cout << " (";
	print_padded_hex(stack_size);
	std::cout << ")";
	std::cout << "  CLK=";
	print_padded_hex(m_computer.GetClock());
	std::cout << "  P=";
	print_padded_hex(m_computer.GetPortIndex());
	std::cout << " ║" << std::endl;
	for (int y = 0; y < i_page_height; ++y) {
		const U16 o = start_i + y * i_page_width;
		
		std::cout << "║ ";
		print_padded_hex(o);
		std::cout << ":";
		for (int x = 0; x < i_page_width; ++x) {
			const XWORD i = m_computer.Peek(o + x);
			if (o + x == m_computer.InstructionPointer()) {
				std::cout << " >";
			} else {
				std::cout << "  ";
			}
			print_padded_hex(i.u);
		}
		
		if (m_computer.InstructionPointer() >= o && m_computer.InstructionPointer() < o + i_page_width) {
			std::cout << "  " << decode(XIS::Enum(m_computer.Peek(m_computer.InstructionPointer()).u)).str;
		} else {
			std::cout << "        ";
		}

		const int display_offset = stack_size < i_page_height ? i_page_height - stack_size : 0;
		if (y - display_offset >= 0) {
			if (y - display_offset == 0) {
				std::cout << " >";
			} else {
				std::cout << "  ";
			}
			print_padded_hex(m_computer.PeekTop(-(y - display_offset)).u);
		} else {
			std::cout << "      ";
		}
		if (y < Computer::NUM_PORTS) {
			if (y == m_computer.GetPortIndex()) {
				std::cout << "                    >";
			} else {
				std::cout << "                     ";
			}
			if (m_computer.GetDeviceAtPort(y) != nullptr) {
				print_padded_hex(m_computer.GetDeviceAtPort(y)->GetHWID());
			} else {
				std::cout << "----";
			}
			std::cout << "   ║" << std::endl;
		} else {
			std::cout << "                            ║" << std::endl;
		}
	}
	std::cout << "╚════════════════════════════════════════════════════════════════════════╝";
}

chars xdebugger::decode(U16 i)
{
	class RenderChars
	{
	private:
		int len(const char *c) {
			int l = 0;
			while (c[l] != 0) { ++l; }
			return l;
		}
	public:
		void operator()(chars &out, const char *c) {
			int l = len(c);
			int i = 0;
			for (; i < l; ++i) {
				out.str[i] = c[i];
			}
			for (; i < sizeof(out.str); ++i) {
				out.str[i] = 0;
			}
		}
	} render;

	chars c;
	switch (i) {
		case XIS::NOP:    render(c, "NOP   "); break;
		case XIS::PUT:    render(c, "PUT   "); break;
		case XIS::PUTS:   render(c, "PUTS  "); break;
		case XIS::PUTI:   render(c, "PUTI  "); break;
		case XIS::CLOCK:  render(c, "CLOCK "); break;
		case XIS::BIN:    render(c, "BIN   "); break;
		case XIS::AT:     render(c, "AT    "); break;
		case XIS::JMP:    render(c, "JMP   "); break;
		case XIS::SKIP:   render(c, "SKIP  "); break;
		case XIS::ADD:    render(c, "ADD   "); break;
		case XIS::SUB:    render(c, "SUB   "); break;
		case XIS::MUL:    render(c, "MUL   "); break;
		case XIS::DIV:    render(c, "DIV   "); break;
		case XIS::MOD:    render(c, "MOD   "); break;
		case XIS::IADD:   render(c, "IADD  "); break;
		case XIS::ISUB:   render(c, "ISUB  "); break;
		case XIS::IMUL:   render(c, "IMUL  "); break;
		case XIS::IDIV:   render(c, "IDIV  "); break;
		case XIS::IMOD:   render(c, "IMOD  "); break;
		case XIS::INEG:   render(c, "INEG  "); break;
		case XIS::LSH:    render(c, "LSH   "); break;
		case XIS::RSH:    render(c, "RSH   "); break;
		case XIS::AND:    render(c, "AND   "); break;
		case XIS::OR:     render(c, "OR    "); break;
		case XIS::XOR:    render(c, "XOR   "); break;
		case XIS::NOT:    render(c, "NOT   "); break;
		case XIS::EQ:     render(c, "EQ    "); break;
		case XIS::NE:     render(c, "NE    "); break;
		case XIS::LE:     render(c, "LE    "); break;
		case XIS::GE:     render(c, "GE    "); break;
		case XIS::LT:     render(c, "LT    "); break;
		case XIS::GT:     render(c, "GT    "); break;
		case XIS::IEQ:    render(c, "IEQ   "); break;
		case XIS::INE:    render(c, "INE   "); break;
		case XIS::ILE:    render(c, "ILE   "); break;
		case XIS::IGE:    render(c, "IGE   "); break;
		case XIS::ILT:    render(c, "ILT   "); break;
		case XIS::IGT:    render(c, "IGT   "); break;
		case XIS::PORT:   render(c, "PORT  "); break;
		case XIS::POLL:   render(c, "POLL  "); break;
		case XIS::PASS:   render(c, "PASS  "); break;
		case XIS::CPUID:  render(c, "CPUID "); break;
		case XIS::PEND:   render(c, "PEND  "); break;
		case XIS::PUSH:   render(c, "PUSH  "); break;
		case XIS::POP:    render(c, "POP   "); break;
		case XIS::TOSS:   render(c, "TOSS  "); break;
		case XIS::MOVD:   render(c, "MOVD  "); break;
		case XIS::MOVU:   render(c, "MOVU  "); break;
		case XIS::PEEK:   render(c, "PEEK  "); break;
		case XIS::HALT:   render(c, "HALT  "); break;
		case XIS::ACK:    render(c, "ACK   "); break;
		case XIS::ERR:    render(c, "ERR   "); break;
		case XIS::CERR:   render(c, "CERR  "); break;
		case XIS::FULL:   render(c, "FULL  "); break;
		case XIS::CJMP:   render(c, "CJMP  "); break;
		case XIS::CSKIP:  render(c, "CSKIP "); break;
		case XIS::CNJMP:  render(c, "CNJMP "); break;
		case XIS::CNSKIP: render(c, "CNSKIP"); break;
		case XIS::DUP:    render(c, "DUP   "); break;
		case XIS::SVA:    render(c, "SVA   "); break;
		case XIS::SVB:    render(c, "SVB   "); break;
		case XIS::SVC:    render(c, "SVC   "); break;
		case XIS::LDA:    render(c, "LDA   "); break;
		case XIS::LDB:    render(c, "LDB   "); break;
		case XIS::LDC:    render(c, "LDC   "); break;
		case XIS::RLA:    render(c, "RLA   "); break;
		case XIS::RLB:    render(c, "RLB   "); break;
		case XIS::RLC:    render(c, "RLC   "); break;
		case XIS::OFA:    render(c, "OFA   "); break;
		case XIS::OFB:    render(c, "OFB   "); break;
		case XIS::OFC:    render(c, "OFC   "); break;
		case XIS::TNS:    render(c, "TNS   "); break;
		case XIS::TUS:    render(c, "TUS   "); break;
		case XIS::TMS:    render(c, "TMS   "); break;
		case XIS::TS:     render(c, "TS    "); break;
		case XIS::TM:     render(c, "TM    "); break;
		case XIS::TH:     render(c, "TH    "); break;
		case XIS::TD:     render(c, "TD    "); break;
		case XIS::TW:     render(c, "TW    "); break;
		default:          render(c, "???   "); break;
	}
	return c;
}

CMD_BEGIN(__debug)
{
	const std::string BIN = buffer_file(std::string(params[0]));
	xdebugger dbg(xcc_binary{ (XWORD*)(BIN.c_str()), unsigned(BIN.size() / sizeof(XWORD)), unsigned(BIN.size() / sizeof(XWORD)) });
	do {
		dbg.ui(40);
		std::cin.get();
	} while (dbg.step());
	return true;
}
CMD_END(__debug, 1, "Debug a specified ROM.", false);

int main(int argc, char **argv)
{
	cmd::Init("computer", "4.2.0");
	return cmd::Process(argc, argv, true);
}
