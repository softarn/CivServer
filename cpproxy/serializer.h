
#ifndef _SERIALIZER_H_
#define _SERIALIZER_H_

#include "bytestream.h"
#include <memory>
//#include <exception>
#include <string>
#include <list>
#include <vector>


namespace proxy
{

	template <typename T>
	class Perhaps
	{
	private:
		T m_data;
		bool m_exists;

	public:
		Perhaps()
			: m_exists(false)
		{
		}


		Perhaps(const T &data)
			: m_data(data),
			  m_exists(true)
		{
		}

		const T *getValue() const
		{
			if(!m_exists)
				return NULL;

			return &m_data;
		}
	};



	/**
	 * The Serializer class converts object to byte arrays and vice versa, useful in network transmission
	 * and file input/output
	 */
	class Serializer
	{
	private:
		ByteStream *m_byteStream;
	public:
		Serializer(ByteStream *byteStream);


		template <typename T> void put(const T &obj);
		template <typename T> void get(T &obj);





		template <typename T> void put(const Perhaps<T> &obj);
		template <typename T> void get(Perhaps<T> &obj);
		template <typename T> void put(const std::vector<T> &obj);
		template <typename T> void get(std::vector<T> &obj);
		template <typename T> void put(const std::list<T> &l);
		template <typename T> void get(std::list<T> &l);

		/**
		 * Get a value from the Serializer
		 * @return The value
		 * @throw SerializerException on failure to load the value
		 */
		template <typename T> T get()
		{
			T t;
			get(t);
			return t;
		}


		template <typename Iterator> void putRange(const Iterator &begin, const Iterator &end)
		{
			int size = 0;

			// Count number of items
			for(Iterator it = begin; it != end; ++it)
				++size;

			// Store the size
			put(size);

			// Store áll elements in the container
			for(Iterator it = begin; it != end; ++it)
				put(*it);
		}

		template <typename T, typename Inserter> void getRange(Inserter it)
		{
			int size = get<int>();

			for(int i = 0; i < size; ++i)
			{
				// Insert into the container
				*it = get<T>();
			}
		}

	};




	template <typename T>
	void Serializer::put(const Perhaps<T> &obj)
	{
		bool exists = obj.getValue() ? true : false;

		put(exists);

		if(exists)
			put(*obj.getValue());
	}

	template <typename T>
	void Serializer::get(Perhaps<T> &obj)
	{
		bool exists = get<bool>();

		if(exists)
			obj = get<T>();
		else
			obj = Perhaps<T>();
	}

	template <typename T>
	void Serializer::put(const std::vector<T> &v)
	{
		putRange(v.begin(), v.end());
	}

	template <typename T>
	void Serializer::get(std::vector<T> &v)
	{
		v.clear();
		getRange<T>(back_inserter(v));
	}


	template <typename T>
	void Serializer::put(const std::list<T> &l)
	{
		putRange(l.begin(), l.end());
	}

	template <typename T>
	void Serializer::get(std::list<T> &l)
	{
		l.clear();
		getRange<T>(back_inserter(l));
	}


}

#endif // _SERIALIZER_H_
