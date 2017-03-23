// Generated by the protocol buffer compiler.  DO NOT EDIT!
// source: information.proto

#ifndef PROTOBUF_information_2eproto__INCLUDED
#define PROTOBUF_information_2eproto__INCLUDED

#include <string>

#include <google/protobuf/stubs/common.h>

#if GOOGLE_PROTOBUF_VERSION < 3000000
#error This file was generated by a newer version of protoc which is
#error incompatible with your Protocol Buffer headers.  Please update
#error your headers.
#endif
#if 3000000 < GOOGLE_PROTOBUF_MIN_PROTOC_VERSION
#error This file was generated by an older version of protoc which is
#error incompatible with your Protocol Buffer headers.  Please
#error regenerate this file with a newer version of protoc.
#endif

#include <google/protobuf/arena.h>
#include <google/protobuf/arenastring.h>
#include <google/protobuf/generated_message_util.h>
#include <google/protobuf/metadata.h>
#include <google/protobuf/message.h>
#include <google/protobuf/repeated_field.h>
#include <google/protobuf/extension_set.h>
#include <google/protobuf/unknown_field_set.h>
// @@protoc_insertion_point(includes)

// Internal implementation detail -- do not call these.
void protobuf_AddDesc_information_2eproto();
void protobuf_AssignDesc_information_2eproto();
void protobuf_ShutdownFile_information_2eproto();

class Exam;
class Grade;
class Person;
class Students_information;

// ===================================================================

class Exam : public ::google::protobuf::Message /* @@protoc_insertion_point(class_definition:Exam) */ {
 public:
  Exam();
  virtual ~Exam();

  Exam(const Exam& from);

  inline Exam& operator=(const Exam& from) {
    CopyFrom(from);
    return *this;
  }

  inline const ::google::protobuf::UnknownFieldSet& unknown_fields() const {
    return _internal_metadata_.unknown_fields();
  }

  inline ::google::protobuf::UnknownFieldSet* mutable_unknown_fields() {
    return _internal_metadata_.mutable_unknown_fields();
  }

  static const ::google::protobuf::Descriptor* descriptor();
  static const Exam& default_instance();

  void Swap(Exam* other);

  // implements Message ----------------------------------------------

  inline Exam* New() const { return New(NULL); }

  Exam* New(::google::protobuf::Arena* arena) const;
  void CopyFrom(const ::google::protobuf::Message& from);
  void MergeFrom(const ::google::protobuf::Message& from);
  void CopyFrom(const Exam& from);
  void MergeFrom(const Exam& from);
  void Clear();
  bool IsInitialized() const;

  int ByteSize() const;
  bool MergePartialFromCodedStream(
      ::google::protobuf::io::CodedInputStream* input);
  void SerializeWithCachedSizes(
      ::google::protobuf::io::CodedOutputStream* output) const;
  ::google::protobuf::uint8* InternalSerializeWithCachedSizesToArray(
      bool deterministic, ::google::protobuf::uint8* output) const;
  ::google::protobuf::uint8* SerializeWithCachedSizesToArray(::google::protobuf::uint8* output) const {
    return InternalSerializeWithCachedSizesToArray(false, output);
  }
  int GetCachedSize() const { return _cached_size_; }
  private:
  void SharedCtor();
  void SharedDtor();
  void SetCachedSize(int size) const;
  void InternalSwap(Exam* other);
  private:
  inline ::google::protobuf::Arena* GetArenaNoVirtual() const {
    return _internal_metadata_.arena();
  }
  inline void* MaybeArenaPtr() const {
    return _internal_metadata_.raw_arena_ptr();
  }
  public:

  ::google::protobuf::Metadata GetMetadata() const;

  // nested types ----------------------------------------------------

  // accessors -------------------------------------------------------

  // optional int32 weight = 2;
  bool has_weight() const;
  void clear_weight();
  static const int kWeightFieldNumber = 2;
  ::google::protobuf::int32 weight() const;
  void set_weight(::google::protobuf::int32 value);

  // optional int32 score = 1;
  bool has_score() const;
  void clear_score();
  static const int kScoreFieldNumber = 1;
  ::google::protobuf::int32 score() const;
  void set_score(::google::protobuf::int32 value);

  // @@protoc_insertion_point(class_scope:Exam)
 private:
  inline void set_has_weight();
  inline void clear_has_weight();
  inline void set_has_score();
  inline void clear_has_score();

  ::google::protobuf::internal::InternalMetadataWithArena _internal_metadata_;
  ::google::protobuf::uint32 _has_bits_[1];
  mutable int _cached_size_;
  ::google::protobuf::int32 weight_;
  ::google::protobuf::int32 score_;
  friend void  protobuf_AddDesc_information_2eproto();
  friend void protobuf_AssignDesc_information_2eproto();
  friend void protobuf_ShutdownFile_information_2eproto();

  void InitAsDefaultInstance();
  static Exam* default_instance_;
};
// -------------------------------------------------------------------

class Grade : public ::google::protobuf::Message /* @@protoc_insertion_point(class_definition:Grade) */ {
 public:
  Grade();
  virtual ~Grade();

  Grade(const Grade& from);

  inline Grade& operator=(const Grade& from) {
    CopyFrom(from);
    return *this;
  }

  inline const ::google::protobuf::UnknownFieldSet& unknown_fields() const {
    return _internal_metadata_.unknown_fields();
  }

  inline ::google::protobuf::UnknownFieldSet* mutable_unknown_fields() {
    return _internal_metadata_.mutable_unknown_fields();
  }

  static const ::google::protobuf::Descriptor* descriptor();
  static const Grade& default_instance();

  void Swap(Grade* other);

  // implements Message ----------------------------------------------

  inline Grade* New() const { return New(NULL); }

  Grade* New(::google::protobuf::Arena* arena) const;
  void CopyFrom(const ::google::protobuf::Message& from);
  void MergeFrom(const ::google::protobuf::Message& from);
  void CopyFrom(const Grade& from);
  void MergeFrom(const Grade& from);
  void Clear();
  bool IsInitialized() const;

  int ByteSize() const;
  bool MergePartialFromCodedStream(
      ::google::protobuf::io::CodedInputStream* input);
  void SerializeWithCachedSizes(
      ::google::protobuf::io::CodedOutputStream* output) const;
  ::google::protobuf::uint8* InternalSerializeWithCachedSizesToArray(
      bool deterministic, ::google::protobuf::uint8* output) const;
  ::google::protobuf::uint8* SerializeWithCachedSizesToArray(::google::protobuf::uint8* output) const {
    return InternalSerializeWithCachedSizesToArray(false, output);
  }
  int GetCachedSize() const { return _cached_size_; }
  private:
  void SharedCtor();
  void SharedDtor();
  void SetCachedSize(int size) const;
  void InternalSwap(Grade* other);
  private:
  inline ::google::protobuf::Arena* GetArenaNoVirtual() const {
    return _internal_metadata_.arena();
  }
  inline void* MaybeArenaPtr() const {
    return _internal_metadata_.raw_arena_ptr();
  }
  public:

  ::google::protobuf::Metadata GetMetadata() const;

  // nested types ----------------------------------------------------

  // accessors -------------------------------------------------------

  // repeated int32 homework = 2;
  int homework_size() const;
  void clear_homework();
  static const int kHomeworkFieldNumber = 2;
  ::google::protobuf::int32 homework(int index) const;
  void set_homework(int index, ::google::protobuf::int32 value);
  void add_homework(::google::protobuf::int32 value);
  const ::google::protobuf::RepeatedField< ::google::protobuf::int32 >&
      homework() const;
  ::google::protobuf::RepeatedField< ::google::protobuf::int32 >*
      mutable_homework();

  // optional .Exam exam = 3;
  bool has_exam() const;
  void clear_exam();
  static const int kExamFieldNumber = 3;
  const ::Exam& exam() const;
  ::Exam* mutable_exam();
  ::Exam* release_exam();
  void set_allocated_exam(::Exam* exam);

  // optional int32 total = 1;
  bool has_total() const;
  void clear_total();
  static const int kTotalFieldNumber = 1;
  ::google::protobuf::int32 total() const;
  void set_total(::google::protobuf::int32 value);

  // @@protoc_insertion_point(class_scope:Grade)
 private:
  inline void set_has_exam();
  inline void clear_has_exam();
  inline void set_has_total();
  inline void clear_has_total();

  ::google::protobuf::internal::InternalMetadataWithArena _internal_metadata_;
  ::google::protobuf::uint32 _has_bits_[1];
  mutable int _cached_size_;
  ::google::protobuf::RepeatedField< ::google::protobuf::int32 > homework_;
  ::Exam* exam_;
  ::google::protobuf::int32 total_;
  friend void  protobuf_AddDesc_information_2eproto();
  friend void protobuf_AssignDesc_information_2eproto();
  friend void protobuf_ShutdownFile_information_2eproto();

  void InitAsDefaultInstance();
  static Grade* default_instance_;
};
// -------------------------------------------------------------------

class Person : public ::google::protobuf::Message /* @@protoc_insertion_point(class_definition:Person) */ {
 public:
  Person();
  virtual ~Person();

  Person(const Person& from);

  inline Person& operator=(const Person& from) {
    CopyFrom(from);
    return *this;
  }

  inline const ::google::protobuf::UnknownFieldSet& unknown_fields() const {
    return _internal_metadata_.unknown_fields();
  }

  inline ::google::protobuf::UnknownFieldSet* mutable_unknown_fields() {
    return _internal_metadata_.mutable_unknown_fields();
  }

  static const ::google::protobuf::Descriptor* descriptor();
  static const Person& default_instance();

  void Swap(Person* other);

  // implements Message ----------------------------------------------

  inline Person* New() const { return New(NULL); }

  Person* New(::google::protobuf::Arena* arena) const;
  void CopyFrom(const ::google::protobuf::Message& from);
  void MergeFrom(const ::google::protobuf::Message& from);
  void CopyFrom(const Person& from);
  void MergeFrom(const Person& from);
  void Clear();
  bool IsInitialized() const;

  int ByteSize() const;
  bool MergePartialFromCodedStream(
      ::google::protobuf::io::CodedInputStream* input);
  void SerializeWithCachedSizes(
      ::google::protobuf::io::CodedOutputStream* output) const;
  ::google::protobuf::uint8* InternalSerializeWithCachedSizesToArray(
      bool deterministic, ::google::protobuf::uint8* output) const;
  ::google::protobuf::uint8* SerializeWithCachedSizesToArray(::google::protobuf::uint8* output) const {
    return InternalSerializeWithCachedSizesToArray(false, output);
  }
  int GetCachedSize() const { return _cached_size_; }
  private:
  void SharedCtor();
  void SharedDtor();
  void SetCachedSize(int size) const;
  void InternalSwap(Person* other);
  private:
  inline ::google::protobuf::Arena* GetArenaNoVirtual() const {
    return _internal_metadata_.arena();
  }
  inline void* MaybeArenaPtr() const {
    return _internal_metadata_.raw_arena_ptr();
  }
  public:

  ::google::protobuf::Metadata GetMetadata() const;

  // nested types ----------------------------------------------------

  // accessors -------------------------------------------------------

  // required string name = 1;
  bool has_name() const;
  void clear_name();
  static const int kNameFieldNumber = 1;
  const ::std::string& name() const;
  void set_name(const ::std::string& value);
  void set_name(const char* value);
  void set_name(const char* value, size_t size);
  ::std::string* mutable_name();
  ::std::string* release_name();
  void set_allocated_name(::std::string* name);

  // optional int32 age = 2;
  bool has_age() const;
  void clear_age();
  static const int kAgeFieldNumber = 2;
  ::google::protobuf::int32 age() const;
  void set_age(::google::protobuf::int32 value);

  // repeated int32 id = 3;
  int id_size() const;
  void clear_id();
  static const int kIdFieldNumber = 3;
  ::google::protobuf::int32 id(int index) const;
  void set_id(int index, ::google::protobuf::int32 value);
  void add_id(::google::protobuf::int32 value);
  const ::google::protobuf::RepeatedField< ::google::protobuf::int32 >&
      id() const;
  ::google::protobuf::RepeatedField< ::google::protobuf::int32 >*
      mutable_id();

  // optional bool registered = 5;
  bool has_registered() const;
  void clear_registered();
  static const int kRegisteredFieldNumber = 5;
  bool registered() const;
  void set_registered(bool value);

  // repeated .Grade grades = 6;
  int grades_size() const;
  void clear_grades();
  static const int kGradesFieldNumber = 6;
  const ::Grade& grades(int index) const;
  ::Grade* mutable_grades(int index);
  ::Grade* add_grades();
  ::google::protobuf::RepeatedPtrField< ::Grade >*
      mutable_grades();
  const ::google::protobuf::RepeatedPtrField< ::Grade >&
      grades() const;

  // @@protoc_insertion_point(class_scope:Person)
 private:
  inline void set_has_name();
  inline void clear_has_name();
  inline void set_has_age();
  inline void clear_has_age();
  inline void set_has_registered();
  inline void clear_has_registered();

  ::google::protobuf::internal::InternalMetadataWithArena _internal_metadata_;
  ::google::protobuf::uint32 _has_bits_[1];
  mutable int _cached_size_;
  ::google::protobuf::internal::ArenaStringPtr name_;
  ::google::protobuf::RepeatedField< ::google::protobuf::int32 > id_;
  ::google::protobuf::int32 age_;
  bool registered_;
  ::google::protobuf::RepeatedPtrField< ::Grade > grades_;
  friend void  protobuf_AddDesc_information_2eproto();
  friend void protobuf_AssignDesc_information_2eproto();
  friend void protobuf_ShutdownFile_information_2eproto();

  void InitAsDefaultInstance();
  static Person* default_instance_;
};
// -------------------------------------------------------------------

class Students_information : public ::google::protobuf::Message /* @@protoc_insertion_point(class_definition:Students_information) */ {
 public:
  Students_information();
  virtual ~Students_information();

  Students_information(const Students_information& from);

  inline Students_information& operator=(const Students_information& from) {
    CopyFrom(from);
    return *this;
  }

  inline const ::google::protobuf::UnknownFieldSet& unknown_fields() const {
    return _internal_metadata_.unknown_fields();
  }

  inline ::google::protobuf::UnknownFieldSet* mutable_unknown_fields() {
    return _internal_metadata_.mutable_unknown_fields();
  }

  static const ::google::protobuf::Descriptor* descriptor();
  static const Students_information& default_instance();

  void Swap(Students_information* other);

  // implements Message ----------------------------------------------

  inline Students_information* New() const { return New(NULL); }

  Students_information* New(::google::protobuf::Arena* arena) const;
  void CopyFrom(const ::google::protobuf::Message& from);
  void MergeFrom(const ::google::protobuf::Message& from);
  void CopyFrom(const Students_information& from);
  void MergeFrom(const Students_information& from);
  void Clear();
  bool IsInitialized() const;

  int ByteSize() const;
  bool MergePartialFromCodedStream(
      ::google::protobuf::io::CodedInputStream* input);
  void SerializeWithCachedSizes(
      ::google::protobuf::io::CodedOutputStream* output) const;
  ::google::protobuf::uint8* InternalSerializeWithCachedSizesToArray(
      bool deterministic, ::google::protobuf::uint8* output) const;
  ::google::protobuf::uint8* SerializeWithCachedSizesToArray(::google::protobuf::uint8* output) const {
    return InternalSerializeWithCachedSizesToArray(false, output);
  }
  int GetCachedSize() const { return _cached_size_; }
  private:
  void SharedCtor();
  void SharedDtor();
  void SetCachedSize(int size) const;
  void InternalSwap(Students_information* other);
  private:
  inline ::google::protobuf::Arena* GetArenaNoVirtual() const {
    return _internal_metadata_.arena();
  }
  inline void* MaybeArenaPtr() const {
    return _internal_metadata_.raw_arena_ptr();
  }
  public:

  ::google::protobuf::Metadata GetMetadata() const;

  // nested types ----------------------------------------------------

  // accessors -------------------------------------------------------

  // repeated .Person person = 1;
  int person_size() const;
  void clear_person();
  static const int kPersonFieldNumber = 1;
  const ::Person& person(int index) const;
  ::Person* mutable_person(int index);
  ::Person* add_person();
  ::google::protobuf::RepeatedPtrField< ::Person >*
      mutable_person();
  const ::google::protobuf::RepeatedPtrField< ::Person >&
      person() const;

  // @@protoc_insertion_point(class_scope:Students_information)
 private:

  ::google::protobuf::internal::InternalMetadataWithArena _internal_metadata_;
  ::google::protobuf::uint32 _has_bits_[1];
  mutable int _cached_size_;
  ::google::protobuf::RepeatedPtrField< ::Person > person_;
  friend void  protobuf_AddDesc_information_2eproto();
  friend void protobuf_AssignDesc_information_2eproto();
  friend void protobuf_ShutdownFile_information_2eproto();

  void InitAsDefaultInstance();
  static Students_information* default_instance_;
};
// ===================================================================


// ===================================================================

#if !PROTOBUF_INLINE_NOT_IN_HEADERS
// Exam

// optional int32 weight = 2;
inline bool Exam::has_weight() const {
  return (_has_bits_[0] & 0x00000001u) != 0;
}
inline void Exam::set_has_weight() {
  _has_bits_[0] |= 0x00000001u;
}
inline void Exam::clear_has_weight() {
  _has_bits_[0] &= ~0x00000001u;
}
inline void Exam::clear_weight() {
  weight_ = 0;
  clear_has_weight();
}
inline ::google::protobuf::int32 Exam::weight() const {
  // @@protoc_insertion_point(field_get:Exam.weight)
  return weight_;
}
inline void Exam::set_weight(::google::protobuf::int32 value) {
  set_has_weight();
  weight_ = value;
  // @@protoc_insertion_point(field_set:Exam.weight)
}

// optional int32 score = 1;
inline bool Exam::has_score() const {
  return (_has_bits_[0] & 0x00000002u) != 0;
}
inline void Exam::set_has_score() {
  _has_bits_[0] |= 0x00000002u;
}
inline void Exam::clear_has_score() {
  _has_bits_[0] &= ~0x00000002u;
}
inline void Exam::clear_score() {
  score_ = 0;
  clear_has_score();
}
inline ::google::protobuf::int32 Exam::score() const {
  // @@protoc_insertion_point(field_get:Exam.score)
  return score_;
}
inline void Exam::set_score(::google::protobuf::int32 value) {
  set_has_score();
  score_ = value;
  // @@protoc_insertion_point(field_set:Exam.score)
}

// -------------------------------------------------------------------

// Grade

// repeated int32 homework = 2;
inline int Grade::homework_size() const {
  return homework_.size();
}
inline void Grade::clear_homework() {
  homework_.Clear();
}
inline ::google::protobuf::int32 Grade::homework(int index) const {
  // @@protoc_insertion_point(field_get:Grade.homework)
  return homework_.Get(index);
}
inline void Grade::set_homework(int index, ::google::protobuf::int32 value) {
  homework_.Set(index, value);
  // @@protoc_insertion_point(field_set:Grade.homework)
}
inline void Grade::add_homework(::google::protobuf::int32 value) {
  homework_.Add(value);
  // @@protoc_insertion_point(field_add:Grade.homework)
}
inline const ::google::protobuf::RepeatedField< ::google::protobuf::int32 >&
Grade::homework() const {
  // @@protoc_insertion_point(field_list:Grade.homework)
  return homework_;
}
inline ::google::protobuf::RepeatedField< ::google::protobuf::int32 >*
Grade::mutable_homework() {
  // @@protoc_insertion_point(field_mutable_list:Grade.homework)
  return &homework_;
}

// optional .Exam exam = 3;
inline bool Grade::has_exam() const {
  return (_has_bits_[0] & 0x00000002u) != 0;
}
inline void Grade::set_has_exam() {
  _has_bits_[0] |= 0x00000002u;
}
inline void Grade::clear_has_exam() {
  _has_bits_[0] &= ~0x00000002u;
}
inline void Grade::clear_exam() {
  if (exam_ != NULL) exam_->::Exam::Clear();
  clear_has_exam();
}
inline const ::Exam& Grade::exam() const {
  // @@protoc_insertion_point(field_get:Grade.exam)
  return exam_ != NULL ? *exam_ : *default_instance_->exam_;
}
inline ::Exam* Grade::mutable_exam() {
  set_has_exam();
  if (exam_ == NULL) {
    exam_ = new ::Exam;
  }
  // @@protoc_insertion_point(field_mutable:Grade.exam)
  return exam_;
}
inline ::Exam* Grade::release_exam() {
  // @@protoc_insertion_point(field_release:Grade.exam)
  clear_has_exam();
  ::Exam* temp = exam_;
  exam_ = NULL;
  return temp;
}
inline void Grade::set_allocated_exam(::Exam* exam) {
  delete exam_;
  exam_ = exam;
  if (exam) {
    set_has_exam();
  } else {
    clear_has_exam();
  }
  // @@protoc_insertion_point(field_set_allocated:Grade.exam)
}

// optional int32 total = 1;
inline bool Grade::has_total() const {
  return (_has_bits_[0] & 0x00000004u) != 0;
}
inline void Grade::set_has_total() {
  _has_bits_[0] |= 0x00000004u;
}
inline void Grade::clear_has_total() {
  _has_bits_[0] &= ~0x00000004u;
}
inline void Grade::clear_total() {
  total_ = 0;
  clear_has_total();
}
inline ::google::protobuf::int32 Grade::total() const {
  // @@protoc_insertion_point(field_get:Grade.total)
  return total_;
}
inline void Grade::set_total(::google::protobuf::int32 value) {
  set_has_total();
  total_ = value;
  // @@protoc_insertion_point(field_set:Grade.total)
}

// -------------------------------------------------------------------

// Person

// required string name = 1;
inline bool Person::has_name() const {
  return (_has_bits_[0] & 0x00000001u) != 0;
}
inline void Person::set_has_name() {
  _has_bits_[0] |= 0x00000001u;
}
inline void Person::clear_has_name() {
  _has_bits_[0] &= ~0x00000001u;
}
inline void Person::clear_name() {
  name_.ClearToEmptyNoArena(&::google::protobuf::internal::GetEmptyStringAlreadyInited());
  clear_has_name();
}
inline const ::std::string& Person::name() const {
  // @@protoc_insertion_point(field_get:Person.name)
  return name_.GetNoArena(&::google::protobuf::internal::GetEmptyStringAlreadyInited());
}
inline void Person::set_name(const ::std::string& value) {
  set_has_name();
  name_.SetNoArena(&::google::protobuf::internal::GetEmptyStringAlreadyInited(), value);
  // @@protoc_insertion_point(field_set:Person.name)
}
inline void Person::set_name(const char* value) {
  set_has_name();
  name_.SetNoArena(&::google::protobuf::internal::GetEmptyStringAlreadyInited(), ::std::string(value));
  // @@protoc_insertion_point(field_set_char:Person.name)
}
inline void Person::set_name(const char* value, size_t size) {
  set_has_name();
  name_.SetNoArena(&::google::protobuf::internal::GetEmptyStringAlreadyInited(),
      ::std::string(reinterpret_cast<const char*>(value), size));
  // @@protoc_insertion_point(field_set_pointer:Person.name)
}
inline ::std::string* Person::mutable_name() {
  set_has_name();
  // @@protoc_insertion_point(field_mutable:Person.name)
  return name_.MutableNoArena(&::google::protobuf::internal::GetEmptyStringAlreadyInited());
}
inline ::std::string* Person::release_name() {
  // @@protoc_insertion_point(field_release:Person.name)
  clear_has_name();
  return name_.ReleaseNoArena(&::google::protobuf::internal::GetEmptyStringAlreadyInited());
}
inline void Person::set_allocated_name(::std::string* name) {
  if (name != NULL) {
    set_has_name();
  } else {
    clear_has_name();
  }
  name_.SetAllocatedNoArena(&::google::protobuf::internal::GetEmptyStringAlreadyInited(), name);
  // @@protoc_insertion_point(field_set_allocated:Person.name)
}

// optional int32 age = 2;
inline bool Person::has_age() const {
  return (_has_bits_[0] & 0x00000002u) != 0;
}
inline void Person::set_has_age() {
  _has_bits_[0] |= 0x00000002u;
}
inline void Person::clear_has_age() {
  _has_bits_[0] &= ~0x00000002u;
}
inline void Person::clear_age() {
  age_ = 0;
  clear_has_age();
}
inline ::google::protobuf::int32 Person::age() const {
  // @@protoc_insertion_point(field_get:Person.age)
  return age_;
}
inline void Person::set_age(::google::protobuf::int32 value) {
  set_has_age();
  age_ = value;
  // @@protoc_insertion_point(field_set:Person.age)
}

// repeated int32 id = 3;
inline int Person::id_size() const {
  return id_.size();
}
inline void Person::clear_id() {
  id_.Clear();
}
inline ::google::protobuf::int32 Person::id(int index) const {
  // @@protoc_insertion_point(field_get:Person.id)
  return id_.Get(index);
}
inline void Person::set_id(int index, ::google::protobuf::int32 value) {
  id_.Set(index, value);
  // @@protoc_insertion_point(field_set:Person.id)
}
inline void Person::add_id(::google::protobuf::int32 value) {
  id_.Add(value);
  // @@protoc_insertion_point(field_add:Person.id)
}
inline const ::google::protobuf::RepeatedField< ::google::protobuf::int32 >&
Person::id() const {
  // @@protoc_insertion_point(field_list:Person.id)
  return id_;
}
inline ::google::protobuf::RepeatedField< ::google::protobuf::int32 >*
Person::mutable_id() {
  // @@protoc_insertion_point(field_mutable_list:Person.id)
  return &id_;
}

// optional bool registered = 5;
inline bool Person::has_registered() const {
  return (_has_bits_[0] & 0x00000008u) != 0;
}
inline void Person::set_has_registered() {
  _has_bits_[0] |= 0x00000008u;
}
inline void Person::clear_has_registered() {
  _has_bits_[0] &= ~0x00000008u;
}
inline void Person::clear_registered() {
  registered_ = false;
  clear_has_registered();
}
inline bool Person::registered() const {
  // @@protoc_insertion_point(field_get:Person.registered)
  return registered_;
}
inline void Person::set_registered(bool value) {
  set_has_registered();
  registered_ = value;
  // @@protoc_insertion_point(field_set:Person.registered)
}

// repeated .Grade grades = 6;
inline int Person::grades_size() const {
  return grades_.size();
}
inline void Person::clear_grades() {
  grades_.Clear();
}
inline const ::Grade& Person::grades(int index) const {
  // @@protoc_insertion_point(field_get:Person.grades)
  return grades_.Get(index);
}
inline ::Grade* Person::mutable_grades(int index) {
  // @@protoc_insertion_point(field_mutable:Person.grades)
  return grades_.Mutable(index);
}
inline ::Grade* Person::add_grades() {
  // @@protoc_insertion_point(field_add:Person.grades)
  return grades_.Add();
}
inline ::google::protobuf::RepeatedPtrField< ::Grade >*
Person::mutable_grades() {
  // @@protoc_insertion_point(field_mutable_list:Person.grades)
  return &grades_;
}
inline const ::google::protobuf::RepeatedPtrField< ::Grade >&
Person::grades() const {
  // @@protoc_insertion_point(field_list:Person.grades)
  return grades_;
}

// -------------------------------------------------------------------

// Students_information

// repeated .Person person = 1;
inline int Students_information::person_size() const {
  return person_.size();
}
inline void Students_information::clear_person() {
  person_.Clear();
}
inline const ::Person& Students_information::person(int index) const {
  // @@protoc_insertion_point(field_get:Students_information.person)
  return person_.Get(index);
}
inline ::Person* Students_information::mutable_person(int index) {
  // @@protoc_insertion_point(field_mutable:Students_information.person)
  return person_.Mutable(index);
}
inline ::Person* Students_information::add_person() {
  // @@protoc_insertion_point(field_add:Students_information.person)
  return person_.Add();
}
inline ::google::protobuf::RepeatedPtrField< ::Person >*
Students_information::mutable_person() {
  // @@protoc_insertion_point(field_mutable_list:Students_information.person)
  return &person_;
}
inline const ::google::protobuf::RepeatedPtrField< ::Person >&
Students_information::person() const {
  // @@protoc_insertion_point(field_list:Students_information.person)
  return person_;
}

#endif  // !PROTOBUF_INLINE_NOT_IN_HEADERS
// -------------------------------------------------------------------

// -------------------------------------------------------------------

// -------------------------------------------------------------------


// @@protoc_insertion_point(namespace_scope)

// @@protoc_insertion_point(global_scope)

#endif  // PROTOBUF_information_2eproto__INCLUDED
